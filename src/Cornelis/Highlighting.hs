{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cornelis.Highlighting where

import           Control.Lens ((<>~))
import           Cornelis.Offsets
import           Cornelis.Pretty
import           Cornelis.Types hiding (Type)
import           Cornelis.Utils (criticalFailure, modifyBufferStuff)
import           Data.Coerce (coerce)
import           Data.IntervalMap.FingerTree (IntervalMap, Interval (Interval))
import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.Map as M
import           Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Traversable (for)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text

hlGroup :: Text -> HighlightGroup
hlGroup "keyword"              = Keyword
hlGroup "symbol"               = Normal
hlGroup "datatype"             = Type
hlGroup "primitivetype"        = Type
hlGroup "function"             = Operator
hlGroup "bound"                = Identifier
hlGroup "inductiveconstructor" = Constant
hlGroup "number"               = Number
hlGroup "comment"              = Comment
hlGroup "hole"                 = Todo
hlGroup "unsolvedmeta"         = Todo
hlGroup "string"               = String
hlGroup "catchallclause"       = Folded
hlGroup "typechecks"           = Normal
hlGroup "module"               = Structure
hlGroup "postulate"            = PreProc
hlGroup "primitive"            = PreProc
hlGroup "error"                = Error
hlGroup _                      = Normal

lineIntervalsForBuffer :: Buffer -> Neovim CornelisEnv LineIntervals
lineIntervalsForBuffer b = do
  buf_lines <- nvim_buf_get_lines b 0 (-1) True
  pure $ getLineIntervals buf_lines

highlightBuffer :: Buffer -> [Highlight] -> Neovim CornelisEnv ()
highlightBuffer b hs = do
  li <- lineIntervalsForBuffer b
  zs <- fmap catMaybes . for hs $ \hl -> do
    ext <- addHighlight b li hl
    pure $ sequenceA (ext, hl_definitionSite hl)
  modifyBufferStuff b $ #bs_goto_sites <>~ M.fromList zs

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
lookupPoint (LineIntervals im) off = do
  (Interval scs _, (startline, s)) <- listToMaybe $ IM.search off im
  pure ( fromIntegral $ getLineNumber startline
       , fromIntegral $ toBytes s (offsetDiff off scs) - 1
       )

------------------------------------------------------------------------------
-- | Vim insists on returning byte-based offsets for the cursor positions...
-- why the fuck? This function undoes the problem.
unvimifyColumn :: Buffer -> (Int64, Int64) -> Neovim env LineOffset
unvimifyColumn b (l, c) = do
  lstr <- buffer_get_line b $ l - 1
  pure $ fromBytes lstr $ fromIntegral c


addHighlight :: Buffer -> LineIntervals -> Highlight -> Neovim CornelisEnv Extmark
addHighlight b lis hl = do
  (start, end)
    <- maybe (criticalFailure "Missing buffer offset when adding highlights") pure
     $ liftA2 (,) (lookupPoint lis (hl_start hl))
     $ lookupPoint lis (hl_end hl)
  setHighlight b start end $ hlGroup $ fromMaybe "" $ listToMaybe $ hl_atoms hl


setHighlight
    :: Buffer
    -> (Int64, Int64)
    -> (Int64, Int64)
    -> HighlightGroup
    -> Neovim CornelisEnv Extmark
setHighlight b (sl, sc) (el, ec) hl = do
  ns <- asks ce_namespace
  fmap coerce $ nvim_buf_set_extmark b ns sl sc $ M.fromList
    [ ( "end_line"
      , ObjectInt el
      )
      -- unlike literally everywhere else in vim, this function is INCLUSIVE
      -- in its end column
    , ( "end_col"
      , ObjectInt $ ec + 1
      )
    , ( "hl_group"
      , ObjectString
          $ encodeUtf8
          $ T.pack
          $ show hl
      )
    ]

