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
import           Neovim.API.String (nvim_buf_get_lines, nvim_create_namespace, nvim_buf_add_highlight)

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

highlightBuffer :: Buffer -> [Highlight] -> Neovim CornelisEnv ()
highlightBuffer b hs = do
  buf_lines <- nvim_buf_get_lines b 0 (-1) True
  for_ hs $ addHighlight b (getLineIntervals buf_lines)

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

lookupLine :: LineIntervals -> Int -> Int -> [(Int64, Int64, Int64)]
lookupLine (LineIntervals im) start end = do
  (Interval scs _, (startline, s)) <- IM.search start im
  (Interval ecs _, endline) <- IM.search end im
  pure ( fromIntegral startline
       , fromIntegral $ measureUtf8 s (start - scs) - 1
       , fromIntegral $ measureUtf8 s (end - ecs) - 1
       )

measureUtf8 :: String -> Int -> Int
measureUtf8 s i = length $ BS.unpack $ encodeUtf8 $ T.pack $ take i s


addHighlight :: Buffer -> LineIntervals -> Highlight -> Neovim CornelisEnv ()
addHighlight b lis hl = do
  ns <- asks ce_namespace
  for_ (lookupLine lis (hl_start hl) (hl_end hl)) $ \(line, start, end) ->
    nvim_buf_add_highlight
      b ns
      (hlGroup $ fromMaybe "" $ listToMaybe $ hl_atoms hl)
      line start end

