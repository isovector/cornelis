{-# LANGUAGE OverloadedStrings #-}

module PropertySpec where

import           Cornelis.Offsets
import           Cornelis.Types
import           Cornelis.Vim
import           Data.Bool (bool)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text
import           Neovim.Test (Seconds(Seconds))
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Utils


spec :: Spec
spec = parallel $ do
  prop "fromBytes is an inverse of toBytes" $ do
    UnicodeString str <- arbitrary
    let len = length str
        t = T.pack str
    i <- suchThat arbitrary (\x -> 0 <= x && x <= len)
    let off = Offset $ fromIntegral i
    pure
      $ counterexample (show str)
      $ counterexample (show i)
      $ fromBytes t (toBytes t off) === off

  prop "goto gets there" $ do
    n <- choose @Int (1, 50)
    strs <- vectorOf n $ listOf agdaChar
    row <- choose (1, length strs)
    let rowidx = row - 1
    col <- choose (0, max 0 $ length (strs !! rowidx) - 1)
    let pn = Pos (LineNumber $ fromIntegral row) $ Offset $ fromIntegral col
    pure
      $ counterexample (show strs)
      $ counterexample (show row)
      $ counterexample (show col)
      $ counterexample (show pn)
      $ counterexample (show $ strs !! rowidx)
      $ withVim (Seconds 1) $ \w b -> do
          buffer_set_lines b 0 (-1) False $ V.fromList $ fmap T.pack $ strs
          setWindowCursor w pn
          ObjectInt row' <- vim_call_function "line" $ V.fromList [ObjectString "."]
          ObjectInt col' <- vim_call_function "virtcol" $ V.fromList [ObjectString "."]
          liftIO $ Row row' `shouldBe` Row (fromIntegral row)
          -- virtcol is 1-based....
          liftIO $ Col (col' - 1) `shouldBe` Col (fromIntegral col)

  prop "replaceInterval does as it says" $ do
    str <- T.pack <$> listOf agdaChar
    rep <- T.pack <$> listOf agdaChar
    start <- choose (0, T.length str - 1)
    end <- choose (0, T.length str - start)
    let srow = LineNumber 1
        scol = Offset $ fromIntegral $ start
        ecol = Offset $ fromIntegral $ start + end
        spn = Pos srow scol
        epn = Pos srow ecol
        expected = T.take start str <> rep <> T.drop (start + end) str
    pure $
      withVim (Seconds 1) $ \_ b -> do
        buffer_set_lines b 0 (-1) False $ V.fromList $ pure str
        intervention b (mapMaybe simplify [Swap str expected]) $
          replaceInterval b spn epn rep


agdaChar :: Gen Char
agdaChar = elements $ mconcat
  [ [' ' .. '~']
  , ['??' .. '??']
  , [ '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '??', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '??', '??', '??', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '??', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '????', '????', '???', '????'
    , '????', '????', '????', '???', '????', '????', '????', '????', '????', '???'
    , '????', '???', '???', '???', '????', '????', '????', '????', '????', '????'
    , '????', '???', '???', '???', '???', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '???', '???', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '\\', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '??', '??'
    , '??', '??', '??', '??', '??', '??', '??', '??', '??', '??'
    , '??', '??', '??', '??', '??', '??', '??', '??', '??', '??'
    , '??', '??', '??', '??', '??', '??', '??', '??', '??', '??'
    , '??', '??', '??', '??', '??', '??', '??', '??', '??', '??'
    , '??', '??', '??', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '???', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '???', '????'
    , '????', '???', '???', '????', '???', '???', '????', '????', '???', '???'
    , '????', '????', '????', '????', '???', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '???', '????', '???'
    , '????', '????', '????', '????', '????', '????', '????', '???', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '???', '????', '????', '????', '????', '???'
    , '???', '????', '????', '????', '????', '????', '????', '????', '????', '???'
    , '????', '????', '????', '????', '????', '????', '????', '???', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '????', '????', '????', '????', '????', '????'
    , '????', '????', '????', '????', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '??', '???', '??', '???', '??', '???', '???'
    , '???', '???', '??', '??', '???', '???', '???', '??', '??', '??'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', '???', '???', '???'
    , '???', '???', '???', '???', '???', '???', '???', ' ', '??', '??'
    , '??', '??', '??', '??', '??'
    ]
  ]


simplify :: Eq a => Edit a -> Maybe (Edit a)
simplify x@(Swap b a) = bool Nothing (Just x) $ b /= a
simplify x = Just x

subsets :: Ord a => [a] -> [[a]]
subsets [] = [[]]
subsets zs@(x : xs) = nubOrd $ filter (/= zs) $ subsets xs ++ fmap (x :) (subsets xs)

newtype Row a = Row a deriving (Eq, Ord, Show)
newtype Col a = Col a deriving (Eq, Ord, Show)

