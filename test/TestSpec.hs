{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import Neovim.API.Text
import Neovim.Test
import Plugin
import Test.Hspec
import Utils
import qualified Data.Text as T


spec :: Spec
spec = do
  diffSpec "should refine" (Seconds 5) "test/Hello.agda"
      [ Modify "unit = ?" "unit = one"] $ \w _ -> do
    nvim_win_set_cursor w (11, 7)
    refine

  let case_split_test name row col =
        diffSpec ("should case split (" <> T.unpack name <> ")") (Seconds 5) "test/Hello.agda"
            (fmap (fmap (name <>))
              [ Modify " x = ?" " true = ?"
              , Insert " false = ?"
              ]
            ) $ \w _ -> do
          nvim_win_set_cursor w (row, col)
          caseSplit "x"
  case_split_test "test" 14 9

  -- TODO(sandy): damn bug in unicde positions AGAIN
  case_split_test "unicodeTest₁" 17 19

