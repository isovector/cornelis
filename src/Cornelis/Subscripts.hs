module Cornelis.Subscripts where

import           Cornelis.Offsets (Offset(Offset))
import           Cornelis.Types (p_line, p_col, Pos' (Pos))
import           Cornelis.Vim (getWindowCursor, getBufferLine, replaceInterval, setWindowCursor, reportError)
import           Data.Foldable (asum, foldl', for_)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import qualified Data.Text as T
import           Data.Void (Void)
import           Neovim (Neovim)
import           Neovim.API.Text (vim_get_current_window, window_get_buffer)
import           Text.Megaparsec

type Parser = Parsec Void T.Text


data Flavor a
  = Digits a
  | Subscript a
  | Superscript a
  deriving (Eq, Ord, Show, Functor)

extract :: Flavor a -> a
extract (Digits a) = a
extract (Subscript a) = a
extract (Superscript a) = a

parseNum :: Num a => Flavor (Char, String) -> Parser (Flavor a)
parseNum f = do
  r <- option id (negate <$ satisfy (== fst (extract f)) ) <*> parseDigits (fmap snd f)
  pure $ r <$ f


parseDigits :: Num a => Flavor String -> Parser a
parseDigits fv = mkNum <$> takeWhile1P (Just "digit") (flip elem $ extract fv)
  where
    mkNum = foldl' step 0 . chunkToTokens (Proxy :: Proxy T.Text)
    step a c = a * 10 + fromIntegral (digitToInt fv c)


digitToInt :: Flavor String -> Char -> Int
digitToInt fv
  = fromMaybe (error "digitToInt: not a digit")
  . flip lookup (zip (extract fv) [0..])


subscripts :: Flavor (Char, String)
subscripts = Subscript ('₋', "₀₁₂₃₄₅₆₇₈₉")


superscripts :: Flavor (Char, String)
superscripts = Superscript ('⁻', "⁰¹²³⁴⁵⁶⁷⁸⁹")


digits :: Flavor (Char, String)
digits = Digits ('-', "0123456789")


mkReplacement :: (Char, String) -> (Char, String) -> Map Char Char
mkReplacement (m1, s1) (m2, s2) = M.fromList $ zip (m1 : s1) (m2 : s2)


replace :: Map Char Char -> String -> String
replace m = fmap (\c -> fromMaybe c $ M.lookup c m)


parseFlavor :: Parser (Flavor Int)
parseFlavor = asum
  [ parseNum digits
  , parseNum superscripts
  , parseNum subscripts
  ]

parseLine :: Parser (String, Flavor Int)
parseLine = manyTill_ anySingle $ try parseFlavor


unparse :: Flavor Int -> String
unparse (Digits n) = unparseWith (extract digits) n
unparse (Subscript n) = unparseWith (extract subscripts) n
unparse (Superscript n) = unparseWith (extract superscripts) n

unparseWith :: (Char, String) -> Int -> String
unparseWith to n =
  let from = extract digits
      rep = mkReplacement from to
   in replace rep $ show n


applyOver :: (Int -> Int) -> Parser (T.Text, (Int, Int))
applyOver f = do
  (start_str, fv) <- parseLine
  let n = unparse $ fmap f fv
      start = T.pack start_str
  pure
    ( T.pack n
    , (T.length start, length $ show $ extract fv)
    )


incNextDigitSeq :: Neovim env ()
incNextDigitSeq = overNextDigitSeq (+1)


decNextDigitSeq :: Neovim env ()
decNextDigitSeq = overNextDigitSeq (subtract 1)


overNextDigitSeq ::  (Int -> Int) -> Neovim env ()
overNextDigitSeq f = do
  w <- vim_get_current_window
  b <- window_get_buffer w
  pos <- getWindowCursor w
  let l = p_line pos
  line <- getBufferLine b l
  let (Offset start_char) = p_col pos

  let (earlier, later) = T.splitAt (fromIntegral start_char) line

  reportError $ T.pack $ show later
  for_ (parse (applyOver f) "" later) $ \(result, (before, target)) ->  do
    reportError result
    let start_offset = T.length earlier + before
        end_offset = start_offset + target
        start_pos = Pos l $ Offset $ fromIntegral start_offset

    replaceInterval b
      start_pos
      (Pos l $ Offset $ fromIntegral end_offset)
      result

    setWindowCursor w start_pos

