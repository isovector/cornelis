module Cornelis.Subscripts where

import           Control.Monad ((<=<))
import           Cornelis.Offsets (Offset(Offset))
import           Cornelis.Types (p_line, p_col, Pos' (Pos))
import           Cornelis.Vim (getWindowCursor, getBufferLine, replaceInterval, setWindowCursor)
import           Data.Foldable (asum)
import           Data.Functor ((<&>))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Neovim (Neovim)
import           Neovim.API.Text (vim_get_current_window, window_get_buffer)


subscripts :: [Char]
subscripts = "₀₁₂₃₄₅₆₇₈₉"

superscripts :: [Char]
superscripts = "⁰¹²³⁴⁵⁶⁷⁸⁹"

digits :: [Char]
digits = "0123456789"

subs :: [Char]
subs = "₋-⁻"


over :: [Char] -> (Int -> Int) -> String -> Maybe String
over lang f s = do
  n <- parse lang s
  pure $ unparse lang $ f n


overAny :: (Int -> Int) -> String -> String
overAny f s = fromMaybe s $ asum
  [ over subscripts f s
  , over superscripts f s
  , over digits f s
  ]


parse :: [Char] -> String -> Maybe Int
parse lang = fromReverseDigits . reverse <=< traverse (flip lookup $ zip lang [0..])


unparse :: [Char] -> Int -> String
unparse lang n = show n <&> \c -> fromMaybe c (lookup c $ zip digits lang )


fromReverseDigits :: [Int] -> Maybe Int
fromReverseDigits [] = Nothing
fromReverseDigits [a] = Just a
fromReverseDigits (a : as) = (a +) <$> fmap (* 10) (fromReverseDigits as)


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

  let is_digit = flip elem (subscripts <> superscripts <> digits)
      (earlier, later) = T.splitAt (fromIntegral start_char) line
      (before, after) = T.break is_digit later
      target = T.takeWhile is_digit after
      result = T.pack $ overAny f $ T.unpack target

      start_offset = T.length earlier + T.length before
      end_offset = start_offset + T.length target

      start_pos = Pos l $ Offset $ fromIntegral start_offset

  replaceInterval b
    start_pos
    (Pos l $ Offset $ fromIntegral end_offset)
    result

  setWindowCursor w start_pos

