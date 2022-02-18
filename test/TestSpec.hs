{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import           Control.Concurrent (threadDelay)
import           Control.Monad (void)
import           Cornelis.Types
import           Cornelis.Utils (withBufferStuff)
import           Cornelis.Vim
import qualified Data.Text as T
import qualified Data.Vector as V
import           Neovim (liftIO)
import           Neovim.API.Text
import           Neovim.Test
import           Plugin
import           Plugin (whyInScope)
import           Test.Hspec
import           Utils


spec :: Spec
spec = do
  diffSpec "should refine" (Seconds 5) "test/Hello.agda"
      [ Modify "unit = ?" "unit = one"] $ \w _ -> do
    goto w 11 8
    refine

  diffSpec "should support helper functions" (Seconds 5) "test/Hello.agda"
      [ Modify "" "test : Unit"] $ \w _ -> do
    goto w 11 8
    helperFunc "test"
    liftIO $ threadDelay 5e5
    void $ vim_command "normal! G\"\"p"

  diffSpec "should case split (unicode lambda)" (Seconds 5) "test/Hello.agda"
      [ Modify "slap = λ { x → ? }" "slap = λ { true → ?"
      , Insert                      "         ; false → ? }"
      ] $ \w _ -> do
    goto w 20 16
    caseSplit "x"

  diffSpec "should preserve indents when doing case split" (Seconds 5) "test/Hello.agda"
      [ Modify "  testIndent b = ?" "  testIndent true = ?"
      , Insert                      "  testIndent false = ?"
      ] $ \w _ -> do
    goto w 24 18
    caseSplit "b"

  diffSpec "should preserve indents when doing case split" (Seconds 5) "test/Hello.agda"
      [ Modify "  testIndent b = ?" "  testIndent true = ?"
      , Insert                      "  testIndent false = ?"
      ] $ \w _ -> do
    goto w 24 18
    caseSplit "b"

  let case_split_test name row col =
        diffSpec ("should case split (" <> T.unpack name <> ")") (Seconds 5) "test/Hello.agda"
            (fmap (fmap (name <>))
              [ Modify " x = ?" " true = ?"
              , Insert " false = ?"
              ]
            ) $ \w _ -> do
          goto w row col
          caseSplit "x"

  case_split_test "test" 14 10
  case_split_test "unicodeTest₁" 17 18

  vimSpec "should support why in scope" (Seconds 5) "test/Hello.agda" $ \_ b -> do
    withBufferStuff b $ \bs -> do
      whyInScope "zero"
      liftIO $ threadDelay 5e5
      res <- buffer_get_lines (iw_buffer $ bs_info_win bs) vimFirstLine vimLastLine False
      liftIO $ V.toList res `shouldContain` ["zero is in scope as"]

  vimSpec "should support goto definition across modules"
          (Seconds 5) "test/Hello.agda" $ \w b -> do
    goto w 2 18
    gotoDefinition
    liftIO $ threadDelay 5e5
    b' <- nvim_get_current_buf
    res <- buffer_get_lines b' vimFirstLine vimLastLine False
    liftIO $ V.toList res `shouldContain` ["module Agda.Builtin.Nat where"]

