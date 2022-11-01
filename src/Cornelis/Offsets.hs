{-# LANGUAGE DataKinds #-}

module Cornelis.Offsets
  ( module Cornelis.Offsets
  , LineNumber(..)
  , Offset(..)
  , BufferOffset
  , LineOffset
  ) where

import           Cornelis.Types
import           Cornelis.Types.Agda
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Data.Int
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)


offsetPlus :: Offset a -> Offset a -> Offset a
offsetPlus = coerce $ (+) @Int32

offsetDiff :: Offset a -> Offset a -> Offset a
offsetDiff = coerce $ (-) @Int32

offsetSubtract :: Int -> Offset a -> Offset a
offsetSubtract i (Offset a) = Offset $ a - fromIntegral i

lineDiff :: LineNumber -> LineNumber -> LineNumber
lineDiff = coerce $ (-) @Int32

incLineNumber :: LineNumber -> LineNumber
incLineNumber = coerce ((+) @Int32 1)

getVimLineNumber :: LineNumber -> Int64
getVimLineNumber (LineNumber l) = fromIntegral l - 1

agdaToLine :: AgdaOffset -> LineOffset
agdaToLine = coerce $ subtract @Int32 1

------------------------------------------------------------------------------
-- | Convert a character-based index into a byte-indexed one
toBytes :: T.Text -> Offset a -> Int
toBytes s (Offset i) = BS.length $ encodeUtf8 $ T.take (fromIntegral i) s

------------------------------------------------------------------------------
-- | Convert a byte-based index into a character-indexed one.
fromBytes :: HasCallStack => T.Text -> Int -> Offset a
fromBytes t i | i < 0 = error $ "from bytes underflow" <> show (t, i)
fromBytes _ 0 = Offset 0
fromBytes t i | Just (c, str) <- T.uncons t =
  let diff = BS.length $ encodeUtf8 $ T.singleton c
   in case i - diff >= 0 of
        True -> Offset $ 1 + coerce (fromBytes str $ i - diff)
        -- We ran out of bytes in the middle of a multibyte character. Just
        -- return the one we're on, and don't underflow!
        False -> Offset 0
fromBytes t i = error $ "missing bytes: " <> show (t, i)

