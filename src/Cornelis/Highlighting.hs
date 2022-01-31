{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cornelis.Highlighting where

import           Cornelis.Offsets
import           Cornelis.Types
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.IntervalMap.FingerTree (IntervalMap, Interval (Interval))
import qualified Data.IntervalMap.FingerTree as IM
import           Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text

hlGroup :: Text -> Text
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
  { li_intervalMap :: IntervalMap BufferOffset (LineNumber, Text)
    -- ^ Mapping from positions to line numbers
  }
  deriving newtype (Semigroup, Monoid)


getLineIntervals :: Vector Text -> LineIntervals
getLineIntervals = LineIntervals . go (Offset 0) (LineNumber 0)
  where
    go :: BufferOffset -> LineNumber -> Vector Text -> IntervalMap BufferOffset (LineNumber, Text)
    go (Offset pos) line v
      | Just (t, ss) <- V.uncons v =
        let len = T.length t
            pos' = pos + fromIntegral len
        in IM.singleton (Interval (coerce pos) $ coerce pos') (line, t)
              <> go (coerce $ pos' + 1) (incLineNumber line) ss
      | otherwise = mempty

lookupPoint :: LineIntervals -> BufferOffset -> Maybe (Int64, Int64)
lookupPoint li i = fmap (\(l, c, _) -> (l, c)) $ listToMaybe $ lookupLine li i i

lookupLine :: LineIntervals -> BufferOffset -> BufferOffset -> [(Int64, Int64, Int64)]
lookupLine (LineIntervals im) start end = do
  (Interval scs _, (startline, s)) <- IM.search start im
  -- TODO(sandy): bug here that doesn't use the end line
  (Interval ecs _, _endline) <- IM.search end im
  pure ( fromIntegral $ getLineNumber startline
       , fromIntegral $ toBytes s (offsetDiff start scs) - 1
       , fromIntegral $ toBytes s (offsetDiff end ecs) - 1
       )

------------------------------------------------------------------------------
-- | Vim insists on returning byte-based offsets for the cursor positions...
-- why the fuck? This function undoes the problem.
unvimifyColumn :: Buffer -> (Int64, Int64) -> Neovim env LineOffset
unvimifyColumn b (l, c) = do
  lstr <- buffer_get_line b $ l - 1
  pure $ fromBytes lstr $ fromIntegral c


addHighlight :: Buffer -> LineIntervals -> Highlight -> Neovim CornelisEnv ()
addHighlight b lis hl = do
  ns <- asks ce_namespace
  for_ (lookupLine lis (hl_start hl) (hl_end hl)) $ \(line, start, end) ->
    nvim_buf_add_highlight
      b ns
      (hlGroup $ fromMaybe "" $ listToMaybe $ hl_atoms hl)
      line start end

