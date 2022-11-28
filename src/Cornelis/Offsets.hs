{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}

module Cornelis.Offsets
  ( Indexing(..)
  , Unit(..)
  , Index()
  , Offset(..)
  , Pos(..)
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
  , toZeroIndexed
  , toOneIndexed
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
  , containsPoint
  , addCol
  ) where

import           Data.Aeson (FromJSON)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           GHC.Show (showSpace)
import           Prettyprinter (Pretty)

-- TODO: Distinguish offsets and positions; offsets have arithmetic, positions don't

-- | Indexing scheme: whether the first index is zero or one.
data Indexing = OneIndexed | ZeroIndexed

-- | What are we counting?
data Unit = Byte | CodePoint | Line

-- | The constructor is hidden, use 'toZeroIndexed' and 'toOneIndexed' to construct it,
-- and 'fromZeroIndexed' and 'fromOneIndexed' to destruct it.
newtype Index (e :: Unit) (i :: Indexing) = Index Int
  deriving newtype (Eq, Ord, Show, Read, FromJSON, Pretty)

type role Index nominal nominal

-- | It doesn't seem worth the trouble to hide this constructor.
newtype Offset (e :: Unit) = Offset Int
  deriving newtype (Eq, Ord, Show, Read, FromJSON, Pretty)

type role Offset nominal

-- | Position in a text file as line-column numbers. This type is indexed by
-- the units of the columns (@Byte@ or @CodePoint@) and by the indexing scheme
-- of lines and columns.
data Pos e i j = Pos
  { p_line :: Index 'Line i
  , p_col :: Index e j
  } deriving (Eq, Ord, Show, Generic)

data Interval p = Interval { iStart, iEnd :: !p }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Show p => Show (Interval p) where
  showsPrec n (Interval s e) =
    showParen (n >= 11) $ showString "Interval " . showsPrec 11 s . showSpace . showsPrec 11 e

-- Common specializations

type LineNumber = Index 'Line

type AgdaIndex = Index 'CodePoint 'OneIndexed
type AgdaOffset = Offset 'CodePoint
type AgdaPos = Pos 'CodePoint 'OneIndexed 'OneIndexed
type AgdaInterval = Interval AgdaPos

type VimIndex = Index 'Byte
type VimOffset = Offset 'Byte
type VimPos = Pos 'Byte 'ZeroIndexed 'ZeroIndexed
type VimInterval = Interval VimPos

-- To pass indices to and from external sources.

-- | Mark a raw index as zero-indexed.
toZeroIndexed :: Integral a => a -> Index e 'ZeroIndexed
toZeroIndexed a = Index (fromIntegral a)

-- | Mark a raw index as one-indexed.
toOneIndexed :: Integral a => a -> Index e 'OneIndexed
toOneIndexed a = Index (fromIntegral a)

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

addCol :: Pos e i j -> Offset e -> Pos e i j
addCol (Pos l c) dc = Pos l (c .+ dc)
