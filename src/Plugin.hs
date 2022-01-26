{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}

module Plugin where

import Control.Lens
import qualified Data.Map as M
import Data.Map (Map)
import Neovim
import Neovim.User.Input (input)
import Neovim.API.String
import Data.Foldable (for_, find)
import Cornelis.Types
import Neovim.Context (gets)
import Control.Monad.State.Class
import Data.Generics.Labels
import Cornelis.Agda (spawnAgda, withCurrentBuffer, runIOTCM)
import Cornelis.Types.Agda
import Data.Traversable (for)



------------------------------------------------------------------------------
-- | Ensure we have a 'BufferStuff' attached to the current buffer.
withAgda :: Neovim CornelisEnv a -> Neovim CornelisEnv a
withAgda m = do
  buffer <- vim_get_current_buffer
  gets (M.lookup buffer . cs_buffers) >>= \case
    Just _ -> m
    Nothing -> do
      agda <- spawnAgda buffer
      modify' $ #cs_buffers %~ M.insert buffer (BufferStuff agda mempty (AllGoalsWarnings [] []) Nothing)
      m

getAgda :: Buffer -> Neovim CornelisEnv Agda
getAgda buffer = gets $ bs_agda_proc . (M.! buffer) . cs_buffers


getGoalAtCursor :: Neovim CornelisEnv (Buffer, Maybe InteractionPoint)
getGoalAtCursor = do
  w <- nvim_get_current_win
  b <- window_get_buffer w
  -- TODO(sandy): stupid off-by-one in vim? or agda?
  (r, (+1) -> c) <- window_get_cursor w
  ips <- fmap bs_ips . M.lookup b <$> gets cs_buffers
  pure (b, flip lookupGoal (r, c) =<< ips)


lookupGoal :: Foldable t => t InteractionPoint -> (Int64, Int64) -> Maybe InteractionPoint
lookupGoal ips rc = flip find ips $ (\(InteractionPoint ip iv) -> containsPoint iv rc)

containsPoint :: IntervalWithoutFile -> (Int64, Int64) -> Bool
containsPoint (Interval s e) (l, c) = and
  [ posLine s <= fromIntegral l
  , fromIntegral l <= posLine e
  , posCol s <= fromIntegral c
  , fromIntegral c <= posCol e
  ]

withGoalAtCursor :: (Buffer -> InteractionPoint -> Neovim CornelisEnv a) -> Neovim CornelisEnv (Maybe a)
withGoalAtCursor f = getGoalAtCursor >>= \case
   (_, Nothing) -> do
     vim_report_error "No goal at cursor"
     pure Nothing
   (b, Just ip) -> fmap Just $ f b ip


load :: CommandArguments -> Neovim CornelisEnv ()
load _ = withAgda $ do
  agda <- withCurrentBuffer getAgda
  name <- buffer_get_name $ a_buffer agda
  flip runIOTCM agda $ Cmd_load name []

solveOne :: CommandArguments -> Neovim CornelisEnv ()
solveOne _ = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  flip runIOTCM agda $ Cmd_solveOne Simplified (InteractionId $ ip_id goal) noRange ""

caseSplit :: CommandArguments -> Neovim CornelisEnv ()
caseSplit _ = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  thing <- input @String "Split on what?" Nothing Nothing
  agda <- getAgda b
  flip runIOTCM agda $ Cmd_make_case (InteractionId $ ip_id goal) noRange thing

