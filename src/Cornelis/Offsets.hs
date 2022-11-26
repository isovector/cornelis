{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Cornelis.Offsets
  ( Indexing(..)
  , Unit(..)
  , Index()
  , Offset(..)
  , TextPos(..)
  , Interval(..)
  , LineNumber
  , AgdaIndex
  , AgdaOffset
  , AgdaPos
  , AgdaInterval
  , VimIndex
  , VimOffset
  , VimPos
  , VimInterval
  , Pos
  , zeroIndexed
  , oneIndexed
  , fromZeroIndexed
  , fromOneIndexed
  , zeroIndex
  , oneIndex
  , incIndex
  , (.+)
  , (.-.)
  , offsetPlus
  , textToBytes
  , charToBytes
  , toBytes
  , fromBytes
  , toAgdaPos
  , fromAgdaPos
  , containsPoint
  , addCol
  ) where

import           Data.Aeson (FromJSON)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Stack (HasCallStack)
import           Prettyprinter (Pretty)

-- TODO: Distinguish offsets and positions; offsets have arithmetic, positions don't

-- | Indexing scheme: whether the first index is zero or one.
data Indexing = OneIndexed | ZeroIndexed

-- | What are we counting?
data Unit = Byte | CodePoint | Line

-- | The constructor is hidden, use 'zeroIndexed' and 'oneIndexed' to construct it,
-- and 'fromZeroIndexed' and 'fromOneIndexed' to destruct it.
newtype Index (e :: Unit) (i :: Indexing) = Index Int
  deriving newtype (Eq, Ord, Show, Read, FromJSON, Pretty)

-- | It doesn't seem worth the trouble to hide this constructor.
newtype Offset (e :: Unit) = Offset Int
  deriving newtype (Eq, Ord, Show, Read, FromJSON, Pretty)

-- | Position in a text file as line-column numbers. This type is indexed by
-- the units of the columns (@Byte@ or @CodePoint@) and by the indexing scheme
-- of lines and columns.
data TextPos e i j = TextPos
  { p_line :: Index 'Line i
  , p_col :: Index e j
  } deriving (Eq, Ord, Show)

data Interval p = Interval { iStart, iEnd :: !p }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Common specializations

type LineNumber = Index 'Line

-- | The main position type in cornelis.
type Pos = TextPos 'CodePoint 'ZeroIndexed 'ZeroIndexed

type AgdaIndex = Index 'CodePoint 'OneIndexed
type AgdaOffset = Offset 'CodePoint
type AgdaPos = TextPos 'CodePoint 'OneIndexed 'OneIndexed
type AgdaInterval = Interval AgdaPos

type VimIndex = Index 'Byte
type VimOffset = Offset 'Byte
type VimPos = TextPos 'Byte 'ZeroIndexed 'ZeroIndexed
type VimInterval = Interval VimPos

-- To pass indices to and from external sources.

-- | Mark a raw index as zero-indexed.
zeroIndexed :: Integral a => a -> Index e 'ZeroIndexed
zeroIndexed a = Index (fromIntegral a)

-- | Mark a raw index as one-indexed.
oneIndexed :: Integral a => a -> Index e 'OneIndexed
oneIndexed a = Index (fromIntegral a)

-- | Unwrap a raw zero-indexed index.
fromZeroIndexed :: Num a => Index e 'ZeroIndexed -> a
fromZeroIndexed (Index a) = fromIntegral a

-- | Unwrap a raw zero-indexed index.
fromOneIndexed :: Num a => Index e 'OneIndexed -> a
fromOneIndexed (Index a) = fromIntegral a

-- | Convert from one- to zero-indexed.
zeroIndex :: Index e 'OneIndexed -> Index e 'ZeroIndexed
zeroIndex (Index i) = Index (i - 1)

-- | Convert from zero- to one-indexed.
oneIndex :: Index e 'ZeroIndexed -> Index e 'OneIndexed
oneIndex (Index i) = Index (i + 1)

-- | Increment index.
incIndex :: Index e i -> Index e i
incIndex (Index i) = Index (i + 1)

-- | Add an offset to an index.
(.+) :: Index e i -> Offset e -> Index e i
Index i .+ Offset n = Index (i + n)

(.-.) :: Index e i -> Index e i -> Offset e
Index i .-. Index j = Offset (i - j)

--

offsetPlus :: Offset a -> Offset a -> Offset a
offsetPlus = coerce $ (+) @Int

--

containsPoint :: Ord p => Interval p -> p -> Bool
containsPoint (Interval s e) p = s <= p && p < e

{-

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
-}

-- | Number of bytes in a 'T.Text'.
textToBytes :: T.Text -> Int
textToBytes t = BS.length (encodeUtf8 t)

-- | Number of bytes in a 'Char'.
charToBytes :: Char -> Int
charToBytes c = textToBytes (T.singleton c)

------------------------------------------------------------------------------
-- | Convert a character-based index into a byte-indexed one
toBytes :: T.Text -> Index 'CodePoint 'ZeroIndexed -> Index 'Byte 'ZeroIndexed
toBytes s (Index i) = Index $ textToBytes $ T.take (fromIntegral i) s

------------------------------------------------------------------------------
-- | Convert a byte-based index into a character-indexed one.
fromBytes :: HasCallStack => T.Text -> Index 'Byte 'ZeroIndexed -> Index 'CodePoint 'ZeroIndexed
fromBytes t (Index i) | i < 0 = error $ "from bytes underflow" <> show (t, i)
fromBytes _ (Index 0) = Index 0
fromBytes t (Index i) | Just (c, str) <- T.uncons t =
  let diff = BS.length $ encodeUtf8 $ T.singleton c
   in case i - diff >= 0 of
        True -> Index $ 1 + coerce (fromBytes str (Index (i - diff)))
        -- We ran out of bytes in the middle of a multibyte character. Just
        -- return the one we're on, and don't underflow!
        False -> Index 0
fromBytes t i = error $ "missing bytes: " <> show (t, i)

toAgdaPos :: Pos -> AgdaPos
toAgdaPos (TextPos l c) = TextPos (oneIndex l) (oneIndex c)

fromAgdaPos :: AgdaPos -> Pos
fromAgdaPos (TextPos l c) = TextPos (zeroIndex l) (zeroIndex c)

addCol :: TextPos e i j -> Offset e -> TextPos e i j
addCol (TextPos l c) dc = TextPos l (c .+ dc)
