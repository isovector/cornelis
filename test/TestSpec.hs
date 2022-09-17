{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import           Control.Concurrent (threadDelay)
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
import           System.Exit (ExitCode(..))
import           System.Process (rawSystem)
import           Test.Hspec
import           Utils

broken :: SpecWith a -> SpecWith a
broken = before_ pending

spec :: Spec
spec = focus $ parallel $ do
  describe "nvim" $ do
    exit_code <- runIO $ rawSystem "nvim" ["--version"]
    it "is executable" $ do
      exit_code `shouldBe` ExitSuccess

  diffSpec "should refine" (Seconds 5) "test/Hello.agda"
      [ Swap "unit = ?" "unit = one"] $ \w _ -> do
    goto w 11 8
    refine

  vimSpec "should support helper functions" (Seconds 5) "test/Hello.agda" $ \w _ -> do
    goto w 11 8
    helperFunc Normalised "help_me"
    liftIO $ threadDelay 5e5
    reg <- getreg "\""
    liftIO $ reg `shouldBe` (Just "help_me : Unit")

  broken $ diffSpec "should case split (unicode lambda)" (Seconds 5) "test/Hello.agda"
      [ Add                       "slap = λ { true → {! !}"
      , Swap "slap = λ { x → ? }" "         ; false → {! !} }"
      ] $ \w _ -> do
    goto w 20 16
    caseSplit "x"

  diffSpec "should preserve indents when doing case split" (Seconds 5) "test/Hello.agda"
      [ Add                       "  testIndent true = {! !}"
      , Swap "  testIndent b = ?" "  testIndent false = {! !}"
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
              [ Add           " true = {! !}"
              , Swap " x = ?" " false = {! !}"
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
          (Seconds 5) "test/Hello.agda" $ \w _ -> do
    goto w 2 18
    gotoDefinition
    liftIO $ threadDelay 5e5
    b' <- nvim_get_current_buf
    res <- buffer_get_lines b' vimFirstLine vimLastLine False
    liftIO $ V.toList res `shouldContain` ["module Agda.Builtin.Nat where"]

  diffSpec "work with multiple" (Seconds 5) "test/Hello.agda"
      [ Add                  "copattern true = {! !}"
      , Swap "copattern = ?" "copattern false = {! !}"
      ] $ \w _ -> do
    goto w 31 13
    caseSplit ""
    liftIO $ threadDelay 5e5
    goto w 31 15
    liftIO $ threadDelay 5e5
    caseSplit "x"

  diffSpec "work with ? in names" (Seconds 5) "test/Hello.agda"
      [ Swap "foo? ?f = {! !}" "foo? ?f x = {! !}"
      ] $ \w _ -> do
    goto w 34 13
    caseSplit ""

  diffSpec "question to meta" (Seconds 5) "test/Hello.agda"
       [ Swap "unit = ?" "unit = {! !}"
       , Swap "test x = ?" "test x = {! !}"
       , Swap "unicodeTest\8321 x = ?" "unicodeTest\8321 x = {! !}"
       , Swap "slap = \955 { x \8594 ? }" "slap = \955 { x \8594 {! !} }"
       , Swap "  testIndent b = ?" "  testIndent b = {! !}"
       , Swap "copattern = ?" "copattern = {! !}"
       ] $ \_ b -> do
    questionToMeta b

  diffSpec "give" (Seconds 5) "test/Hello.agda"
        [ Swap "give = {! true !}" "give = true"
        ] $ \w _ -> do
    goto w 37 11
    give

  let elaborate_test rw changes row column =
        let title = maybe "default mode" show rw in
        diffSpec ("elaborate (" <> title <> ")") (Seconds 5) "test/Hello.agda" changes $
            \w _ -> do
                goto w row column
                withNormalizationMode rw elaborate

  elaborate_test (Just "Normalised")
    [Swap "elaborate = {! not true !}" "elaborate = false"]
    44 16

  elaborate_test (Just "AsIs")
    [Swap "elaborate = {! not true !}" "elaborate = not true"]
    44 16
