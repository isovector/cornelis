{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Plugin where

import           Control.Lens
import           Control.Monad.State.Class
import           Cornelis.Agda (spawnAgda, withCurrentBuffer, runIOTCM)
import           Cornelis.Highlighting (unvimifyColumn)
import           Cornelis.InfoWin (buildInfoBuffer, showInfoWindow)
import           Cornelis.Offsets
import           Cornelis.Pretty (prettyGoals)
import           Cornelis.Types
import           Cornelis.Types.Agda hiding (Error)
import           Cornelis.Utils
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text
import           Neovim.User.Input (input)



------------------------------------------------------------------------------
-- | Ensure we have a 'BufferStuff' attached to the current buffer.
withAgda :: Neovim CornelisEnv a -> Neovim CornelisEnv a
withAgda m = do
  buffer <- vim_get_current_buffer
  gets (M.lookup buffer . cs_buffers) >>= \case
    Just _ -> m
    Nothing -> do
      agda <- spawnAgda buffer
      iw <- buildInfoBuffer
      modify' $ #cs_buffers %~ M.insert buffer BufferStuff
        { bs_agda_proc = agda
        , bs_ips = mempty
        , bs_goto_sites = mempty
        , bs_goals = AllGoalsWarnings [] [] [] []
        , bs_info_win = iw
        }
      m

getAgda :: Buffer -> Neovim CornelisEnv Agda
getAgda buffer = gets $ bs_agda_proc . (M.! buffer) . cs_buffers


getGoalAtCursor :: Neovim CornelisEnv (Buffer, Maybe InteractionPoint)
getGoalAtCursor = do
  w <- nvim_get_current_win
  b <- window_get_buffer w
  (r, c) <- window_get_cursor w
  c' <- unvimifyColumn b (r, c)
  ips <- fmap bs_ips . M.lookup b <$> gets cs_buffers
  -- TODO(sandy): stupid off-by-one in vim? or agda?
  pure (b, ips >>= \ip -> lookupGoal ip (LineNumber $ fromIntegral r) (offsetPlus c' $ Offset 1))


lookupGoal :: Foldable t => t InteractionPoint -> LineNumber -> LineOffset -> Maybe InteractionPoint
lookupGoal ips line col = flip find ips $ (\(InteractionPoint _ iv) -> containsPoint iv line col)

containsPoint :: IntervalWithoutFile -> LineNumber -> LineOffset -> Bool
containsPoint (Interval s e) l c = and
  [ posLine s <= l
  , l <= posLine e
  , posCol s <= c
  , c <= posCol e
  ]

-- TODO(sandy): There's a bug here! Vim reports byte-offset columns, so lines
-- that contain unicode are wrong. ffs
withGoalAtCursor :: (Buffer -> InteractionPoint -> Neovim CornelisEnv a) -> Neovim CornelisEnv (Maybe a)
withGoalAtCursor f = getGoalAtCursor >>= \case
   (_, Nothing) -> do
     vim_report_error "No goal at cursor"
     pure Nothing
   (b, Just ip) -> fmap Just $ f b ip


getExtmark :: Buffer -> (Int64, Int64) -> Neovim CornelisEnv (Maybe Extmark)
getExtmark b (r, c) = do
  ns <- asks ce_namespace
  -- another thing off by 1??
  let pos = ObjectArray [ObjectInt $ r - 1, ObjectInt c]
  res <- nvim_buf_get_extmarks b ns pos pos mempty
  pure $ case res V.!? 0 of
    Just (ObjectArray (ObjectUInt ext : _)) -> pure $ Extmark $ fromIntegral ext
    _ -> Nothing

gotoDefinition :: CommandArguments -> Neovim CornelisEnv ()
gotoDefinition _ = withAgda $ do
  w <- nvim_get_current_win
  rc <- window_get_cursor w
  b <- window_get_buffer w
  withBufferStuff b $ \bs -> do
    getExtmark b rc >>= \case
      Nothing -> vim_out_write "No syntax under cursor."
      Just ex -> do
        case M.lookup ex $ bs_goto_sites bs of
          Nothing -> do
            vim_out_write "No definition under cursor."
          Just ds -> do
            -- TODO(sandy): escape spaces
            vim_command $ "edit " <> ds_filepath ds
            b' <- window_get_buffer w
            contents <- fmap (T.unlines . V.toList) $ buffer_get_lines b' 0 (-1) False
            let buffer_idx = toBytes contents $ ds_position ds
            -- TODO(sandy): use window_set_cursor instead?
            vim_command $ "normal! " <> T.pack (show buffer_idx) <> "go"


doLoad :: CommandArguments -> Neovim CornelisEnv ()
doLoad = const load

load :: Neovim CornelisEnv ()
load = withAgda $ do
  agda <- withCurrentBuffer getAgda
  name <- buffer_get_name $ a_buffer agda
  flip runIOTCM agda $ Cmd_load name []

doAllGoals :: CommandArguments -> Neovim CornelisEnv ()
doAllGoals = const allGoals

allGoals :: Neovim CornelisEnv ()
allGoals =
  withAgda $ withCurrentBuffer $ \b ->
    withBufferStuff b $ \bs -> do
      goalWindow b $ bs_goals bs


solveOne :: CommandArguments -> Neovim CornelisEnv ()
solveOne _ = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  flip runIOTCM agda $ Cmd_solveOne Simplified (InteractionId $ ip_id goal) noRange ""

typeContext :: CommandArguments -> Neovim CornelisEnv ()
typeContext _ = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  flip runIOTCM agda $ Cmd_goal_type_context Simplified (InteractionId $ ip_id goal) noRange ""

doRefine :: CommandArguments -> Neovim CornelisEnv ()
doRefine = const refine

refine :: Neovim CornelisEnv ()
refine = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  flip runIOTCM agda $ Cmd_refine_or_intro True (InteractionId $ ip_id goal) noRange ""

doCaseSplit :: CommandArguments -> Neovim CornelisEnv ()
doCaseSplit _ = do
  thing <- input @Text "Split on what?" Nothing Nothing
  caseSplit thing

caseSplit :: Text -> Neovim CornelisEnv ()
caseSplit thing = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  flip runIOTCM agda $ Cmd_make_case (InteractionId $ ip_id goal) noRange $ T.unpack thing

goalWindow :: Buffer -> DisplayInfo ->  Neovim CornelisEnv ()
goalWindow b = showInfoWindow b . prettyGoals

