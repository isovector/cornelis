module Cornelis.Offsets
  ( module Cornelis.Offsets
  , LineNumber(..)
  , Offset(..)
  , BufferOffset
  , LineOffset
  ) where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Cornelis.Types.Agda
import Data.Coerce (coerce)
import Data.Int


offsetPlus :: Offset a -> Offset a -> Offset a
offsetPlus = coerce $ (+) @Int32

offsetDiff :: Offset a -> Offset a -> Offset a
offsetDiff = coerce $ subtract @Int32

incLineNumber :: LineNumber -> LineNumber
incLineNumber = coerce ((+) @Int32 1)

------------------------------------------------------------------------------
-- | Convert a character-based index into a byte-indexed one
toBytes :: T.Text -> Offset a -> Int
toBytes s (Offset i) = BS.length $ encodeUtf8 $ T.take (fromIntegral i) s

------------------------------------------------------------------------------
-- | Convert a byte-based index into a character-indexed one.
fromBytes :: T.Text -> Int -> Offset a
fromBytes _ 0 = Offset 0
fromBytes t i | Just (c, str) <- T.uncons t =
  Offset $ 1 + coerce (fromBytes str $ i - (BS.length $ encodeUtf8 $ T.singleton c))
-- TODO(sandy): ??? maybe crash?
fromBytes _ i = Offset $ fromIntegral i

