{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Goals where

import           Control.Arrow ((&&&))
import           Control.Lens
import           Control.Monad.State.Class
import           Cornelis.Agda (withAgda)
import           Cornelis.Offsets
import           Cornelis.Types
import           Cornelis.Types.Agda
import           Cornelis.Utils
import           Cornelis.Vim
import           Data.Foldable (toList)
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Text as T
import           Neovim
import           Neovim.API.Text


-- | Find a goal in the current window
findGoal :: Ord a => (Pos -> Pos -> Maybe a) -> Neovim CornelisEnv ()
findGoal hunt = withAgda $ do
  w <- vim_get_current_window
  b <- window_get_buffer w
  withBufferStuff b $ \bs -> do
    pos <- getWindowCursor w
    let goals = toList $ bs_ips bs
        judged_goals
              = mapMaybe ( sequenceA
                         . (id &&& hunt pos)
                         . positionToPos
                         . iStart
                         . ip_interval
                         ) goals
    case judged_goals of
      [] -> reportInfo "No hole matching predicate"
      _ -> do
        let pos' = fst $ maximumBy (comparing snd) judged_goals
        setWindowCursor w pos'

prevGoal :: Neovim CornelisEnv ()
prevGoal =
  findGoal $ \pos goal ->
    case pos > goal of
      False -> Nothing
      True -> Just $ ( lineDiff (p_line goal) (p_line pos)
                     , offsetDiff (p_col goal) (p_col pos)
                     )

nextGoal :: Neovim CornelisEnv ()
nextGoal =
  findGoal $ \pos goal ->
    case pos < goal of
      False -> Nothing
      True -> Just $ Down ( lineDiff (p_line goal) (p_line pos)
                          , offsetDiff (p_col goal) (p_col pos)
                          )

getGoalAtCursor :: Neovim CornelisEnv (Buffer, Maybe (InteractionPoint Identity LineOffset))
getGoalAtCursor = do
  w <- nvim_get_current_win
  b <- window_get_buffer w
  p <- getWindowCursor w
  ips <- fmap bs_ips . M.lookup b <$> gets cs_buffers
  pure (b, ips >>= flip lookupGoal p)


lookupGoal :: Foldable t => t (InteractionPoint Identity LineOffset) -> Pos -> Maybe (InteractionPoint Identity LineOffset)
lookupGoal ips p = flip find ips $ (\(InteractionPoint _ (Identity iv)) -> containsPoint iv p)


withGoalAtCursor :: (Buffer -> InteractionPoint Identity LineOffset -> Neovim CornelisEnv a) -> Neovim CornelisEnv (Maybe a)
withGoalAtCursor f = getGoalAtCursor >>= \case
   (_, Nothing) -> do
     reportInfo "No goal at cursor"
     pure Nothing
   (b, Just ip) -> fmap Just $ f b ip


getGoalContents_maybe :: Buffer -> InteractionPoint Identity LineOffset -> Neovim CornelisEnv (Maybe Text)
getGoalContents_maybe b ip = do
  iv <- fmap T.strip $ getBufferInterval b $ ip_interval ip
  traceMX "iv" $ show iv
  pure $ case iv of
    "?" -> Nothing
         -- Chop off {!, !} and trim any spaces.
    _ -> Just $ T.strip $ T.dropEnd 2 $ T.drop 2 $ iv

getGoalContents :: Buffer -> InteractionPoint Identity LineOffset -> Neovim CornelisEnv Text
getGoalContents b ip = fromMaybe "" <$> getGoalContents_maybe b ip

