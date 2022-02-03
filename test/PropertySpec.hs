module PropertySpec where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Cornelis.Offsets
import qualified Data.Text as T


spec :: Spec
spec = do
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

