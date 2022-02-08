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

positionToPos :: Position' LineOffset () -> Pos
positionToPos (Pn _ _ ln off') = Pos {p_line = ln, p_col = off'}

posToPosition :: Pos -> Position' LineOffset ()
posToPosition (Pos l c) = Pn () (Offset $ error "fake buffer offset") l c


offsetPlus :: Offset a -> Offset a -> Offset a
offsetPlus = coerce $ (+) @Int32

offsetDiff :: Offset a -> Offset a -> Offset a
offsetDiff = coerce $ (-) @Int32

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
fromBytes :: T.Text -> Int -> Offset a
fromBytes _ 0 = Offset 0
fromBytes t i | Just (c, str) <- T.uncons t =
  Offset $ 1 + coerce (fromBytes str $ i - (BS.length $ encodeUtf8 $ T.singleton c))
-- TODO(sandy): ??? maybe crash?
fromBytes _ _ = error "missing bytes"

