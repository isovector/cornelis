{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import qualified Data.Text as T
import           Neovim.Test
import           Plugin
import           Test.Hspec
import           Utils


spec :: Spec
spec = do
  diffSpec "should refine" (Seconds 5) "test/Hello.agda"
      [ Modify "unit = ?" "unit = one"] $ \w _ -> do
    goto w 11 8
    refine

  diffSpec "should case split (unicode lambda)" (Seconds 5) "test/Hello.agda"
      [ Modify "slap = λ { x → ? }" "slap = λ { true → ?"
      , Insert                      "         ; false → ? }"
      ] $ \w _ -> do
    goto w 20 16
    caseSplit "x"

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

