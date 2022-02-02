{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import Neovim.API.Text
import Neovim.Test
import Plugin
import Test.Hspec
import Utils
import qualified Data.Text as T
import Lib
import Cornelis.Types.Agda
import Control.Lens
import Cornelis.Offsets (offsetDiff)
import Data.Int (Int32)
import Neovim (Neovim)


mkPos :: Int32 -> Int32 -> Position' LineOffset ()
mkPos line col = Pn () (Offset 0) (LineNumber $ line) $ Offset col

goto :: Window -> Buffer -> Int32 -> Int32 -> Neovim env ()
goto  w b row col = do
  (row', col') <- fmap positionToVim $ vimifyPositionM b $ mkPos row col
  -- TODO(sandy): I can't keep track of these one indicies for my life
  nvim_win_set_cursor w (row' + 1, col')


spec :: Spec
spec = do
  diffSpec "should refine" (Seconds 5) "test/Hello.agda"
      [ Modify "unit = ?" "unit = one"] $ \w b -> do
    goto w b 11 7
    refine

  let case_split_test name row col =
        diffSpec ("should case split (" <> T.unpack name <> ")") (Seconds 5) "test/Hello.agda"
            (fmap (fmap (name <>))
              [ Modify " x = ?" " true = ?"
              , Insert " false = ?"
              ]
            ) $ \w b -> do
          goto w b row col
          caseSplit "x"

  case_split_test "test" 14 10
  case_split_test "unicodeTest₁" 17 17

