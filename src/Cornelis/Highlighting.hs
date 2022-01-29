{-# LANGUAGE DerivingStrategies #-}

module Cornelis.Highlighting where

import           Cornelis.Types
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.IntervalMap.FingerTree (IntervalMap, Interval (Interval))
import qualified Data.IntervalMap.FingerTree as IM
import           Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Neovim
import           Neovim.API.String (nvim_buf_get_lines, nvim_create_namespace, nvim_buf_add_highlight, buffer_get_line, vim_report_error)

hlGroup :: String -> String
hlGroup "keyword"              = "Keyword"
hlGroup "symbol"               = "Normal"
hlGroup "datatype"             = "Type"
hlGroup "primitivetype"        = "Type"
hlGroup "function"             = "Operator"
hlGroup "bound"                = "Identifier"
hlGroup "inductiveconstructor" = "Constant"
hlGroup "number"               = "Number"
hlGroup "comment"              = "Comment"
hlGroup "hole"                 = "Todo"
hlGroup "unsolvedmeta"         = "Todo"
hlGroup "string"               = "String"
hlGroup "catchallclause"       = "Folded"
hlGroup "typechecks"           = "Normal"
hlGroup "module"               = "Structure"
hlGroup "postulate"            = "PreProc"
hlGroup "primitive"            = "PreProc"
hlGroup "error"                = "Error"
hlGroup _                      = "Normal"

lineIntervalsForBuffer :: Buffer -> Neovim CornelisEnv LineIntervals
lineIntervalsForBuffer b = do
  buf_lines <- nvim_buf_get_lines b 0 (-1) True
  pure $ getLineIntervals buf_lines

highlightBuffer :: Buffer -> [Highlight] -> Neovim CornelisEnv ()
highlightBuffer b hs = do
  li <- lineIntervalsForBuffer b
  for_ hs $ addHighlight b li

newtype LineIntervals = LineIntervals
  { li_intervalMap :: IntervalMap Int (Int, String)
    -- ^ Mapping from positions to line numbers
  }
  deriving newtype (Semigroup, Monoid)


getLineIntervals :: [String] -> LineIntervals
getLineIntervals = LineIntervals . go 0 0
  where
    go _ _ [] = mempty
    go pos line (s : ss) =
      let len = length s
       in IM.singleton (Interval pos $ pos + len) (line, s) <> go (pos + len + 1) (line + 1) ss

lookupPoint :: LineIntervals -> Int -> Maybe (Int64, Int64)
lookupPoint li i = fmap (\(l, c, _) -> (l, c)) $ listToMaybe $ lookupLine li i i

lookupLine :: LineIntervals -> Int -> Int -> [(Int64, Int64, Int64)]
lookupLine (LineIntervals im) start end = do
  (Interval scs _, (startline, s)) <- IM.search start im
  (Interval ecs _, endline) <- IM.search end im
  pure ( fromIntegral startline
       , fromIntegral $ measureUtf8 s (start - scs) - 1
       , fromIntegral $ measureUtf8 s (end - ecs) - 1
       )

------------------------------------------------------------------------------
-- | Vim insists on returning byte-based offsets for the cursor positions...
-- why the fuck? This function (expensively) undoes the problem.
unvimifyColumn :: Buffer -> (Int64, Int64) -> Neovim env Int
unvimifyColumn b (l, c) = do
  lstr <- buffer_get_line b $ l - 1
  pure $ unmeasureUtf8 lstr $ fromIntegral c

------------------------------------------------------------------------------
-- | Convert a character-based index into a byte-indexed one
measureUtf8 :: String -> Int -> Int
measureUtf8 s i = length $ BS.unpack $ encodeUtf8 $ T.pack $ take i s

------------------------------------------------------------------------------
-- | Convert a byte-based index into a character-indexed one.
--
-- Stupidly expensive
unmeasureUtf8 :: String -> Int -> Int
unmeasureUtf8 _ 0 = 0
unmeasureUtf8 (c : str) i = 1 + (unmeasureUtf8 str $ i - (length $ BS.unpack $ encodeUtf8 $ T.pack $ pure c))
-- TODO(sandy): ??? maybe crash?
unmeasureUtf8 [] i = i


addHighlight :: Buffer -> LineIntervals -> Highlight -> Neovim CornelisEnv ()
addHighlight b lis hl = do
  ns <- asks ce_namespace
  for_ (lookupLine lis (hl_start hl) (hl_end hl)) $ \(line, start, end) ->
    nvim_buf_add_highlight
      b ns
      (hlGroup $ fromMaybe "" $ listToMaybe $ hl_atoms hl)
      line start end

