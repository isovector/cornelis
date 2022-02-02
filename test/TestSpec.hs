{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import Neovim.API.Text
import Neovim.Test
import Plugin
import Test.Hspec
import Utils


spec :: Spec
spec = do
  diffSpec "should refine" (Seconds 5) "test/Hello.agda"
      [ Modify "unit = ?" "unit = one"] $ \w _ -> do
    nvim_win_set_cursor w (11, 7)
    refine

  diffSpec "should case split" (Seconds 5) "test/Hello.agda"
      [ Modify "test x = ?" "test true = ?"
      , Insert "test false = ?"
      ] $ \w _ -> do
    nvim_win_set_cursor w (14, 9)
    caseSplit "x"

