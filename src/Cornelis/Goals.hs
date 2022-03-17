{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Goals where

import Control.Arrow ((&&&))
import Cornelis.Offsets
import Cornelis.Types
import Cornelis.Types.Agda
import Cornelis.Utils
import Cornelis.Vim
import Data.Foldable (toList, maximumBy)
import Data.Maybe
import Data.Ord (comparing, Down (Down))
import Neovim
import Neovim.API.Text
import Plugin

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

