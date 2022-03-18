{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import           Control.Concurrent (threadDelay)
import           Control.Monad (void)
import           Cornelis.Types
import           Cornelis.Types.Agda (Rewrite (..))
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


broken :: SpecWith a -> SpecWith a
broken = before_ pending

spec :: Spec
spec = parallel $ do
  diffSpec "should refine" (Seconds 5) "test/Hello.agda"
      [ Swap "unit = {! !}" "unit = one"] $ \w _ -> do
    goto w 11 8
    refine

  diffSpec "should support helper functions" (Seconds 5) "test/Hello.agda"
      [ Swap "" "help_me : Unit"] $ \w _ -> do
    goto w 11 8
    helperFunc Normalised "help_me"
    liftIO $ threadDelay 5e5
    void $ vim_command "normal! G\"\"p"

  diffSpec "should case split (unicode lambda)" (Seconds 5) "test/Hello.agda"
      [ Add                           "slap = λ { true → {! !}"
      , Swap "slap = λ { x → {! !} }" "         ; false → {! !} }"
      ] $ \w _ -> do
    goto w 20 16
    caseSplit "x"

  diffSpec "should preserve indents when doing case split" (Seconds 5) "test/Hello.agda"
      [ Add                           "  testIndent true = {! !}"
      , Swap "  testIndent b = {! !}" "  testIndent false = {! !}"
      ] $ \w _ -> do
    goto w 24 18
    caseSplit "b"

  diffSpec "should refine with hints" (Seconds 5) "test/Hello.agda"
      [ Swap "isEven∘ (suc n) = {! isEven∘ !}" "isEven∘ (suc n) = isEven∘ {! !}"] $ \w _ -> do
    goto w 28 24
    refine

  let case_split_test name row col =
        diffSpec ("should case split (" <> T.unpack name <> ")") (Seconds 5) "test/Hello.agda"
            (fmap (fmap (name <>))
              [ Add " true = {! !}"
              , Swap " x = {! !}" " false = {! !}"
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

