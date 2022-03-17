{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns #-}

module Cornelis.Highlighting where

import           Control.Lens ((<>~))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe
import           Cornelis.Offsets
import           Cornelis.Pretty
import           Cornelis.Types hiding (Type)
import           Cornelis.Utils
import           Cornelis.Vim (unvimifyColumn, vimifyPositionM)
import           Data.Bifunctor (first)
import           Data.Coerce (coerce)
import           Data.IntervalMap.FingerTree (IntervalMap)
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

holeHlGroup :: HighlightGroup
holeHlGroup = Todo

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
hlGroup "hole"                 = holeHlGroup
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

highlightBuffer :: Buffer -> [Highlight] -> Neovim CornelisEnv [Interval' Int64]
highlightBuffer b hs = do
  li <- lineIntervalsForBuffer b
  (holes, exts) <- fmap unzip $ for hs $ \hl -> do
    (hole, mext) <- addHighlight b li hl
    pure (hole, (hl, mext))

  let zs = catMaybes $ do
        (hl, mext) <- exts
        pure $ do
          ext <- mext
          ds <- hl_definitionSite hl
          pure (ext, ds)

  modifyBufferStuff b $ #bs_goto_sites <>~ M.fromList zs
  pure $ concat holes

newtype LineIntervals = LineIntervals
  { li_intervalMap :: IntervalMap BufferOffset (LineNumber, Text)
    -- ^ Mapping from positions to line numbers
  }
  deriving newtype (Semigroup, Monoid)


getLineIntervals :: Vector Text -> LineIntervals
getLineIntervals = LineIntervals . go (Offset 0) (LineNumber 0)
  where
    go
        :: BufferOffset
        -> LineNumber
        -> Vector Text
        -> IntervalMap BufferOffset (LineNumber, Text)
    go (Offset pos) line v
      | Just (t, ss) <- V.uncons v =
        let len = T.length t
            pos' = pos + fromIntegral len
        in IM.singleton (IM.Interval (coerce pos) $ coerce pos') (line, t)
              <> go (coerce $ pos' + 1) (incLineNumber line) ss
      | otherwise = mempty

lookupPoint :: LineIntervals -> BufferOffset -> Maybe (LineNumber, Int64)
lookupPoint (LineIntervals im) off = do
  (IM.Interval scs _, (startline, s)) <- listToMaybe $ IM.search off im
  pure ( startline
       , fromIntegral $ toBytes s (offsetDiff off scs) - 1
       )


------------------------------------------------------------------------------
-- | Returns any holes it tried to highlight on the left
addHighlight
    :: Buffer
    -> LineIntervals
    -> Highlight
    -> Neovim CornelisEnv ([Interval' Int64], Maybe Extmark)
addHighlight b lis hl = do
  case (,)
          <$> lookupPoint lis (hl_start hl)
               -- Subtract 1 here from the end offset because unlike everywhere
               -- else in vim, extmark ranges are inclusive...
           <*> lookupPoint lis (offsetDiff (hl_end hl) (Offset 1)) of
    Just (start@(sl, sc), end@(el, ec)) -> do
      let atom = fromMaybe "" $ listToMaybe $ hl_atoms hl
      ext <- setHighlight b (first (fromIntegral . getOneIndexedLineNumber) start)
                            (first (fromIntegral . getOneIndexedLineNumber) end)
                          $ hlGroup atom

      pure $ (, Just ext) $ case atom == "hole" of
        False -> []
        True ->
          pure $ Interval
            { iStart = Pos (incLineNumber sl) sc
            , iEnd   = Pos (incLineNumber el) (ec + 1)
            }
    Nothing -> pure ([], Nothing)


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

highlightInterval
    :: Buffer
    -> Interval' LineOffset
    -> HighlightGroup
    -> Neovim CornelisEnv Extmark
highlightInterval b int hl = do
  Interval (Pos sl sc) (Pos el ec) <- traverseInterval (vimifyPositionM b) int
  let to_vim = subtract 1 . fromIntegral . getOneIndexedLineNumber
  setHighlight b (to_vim sl, sc) (to_vim el, ec) hl


parseExtmark :: Buffer -> Object -> Neovim CornelisEnv (Maybe ExtmarkStuff)
parseExtmark b
  (ObjectArray ( (objectToInt -> Just ext)
               : (objectToInt -> Just line)
               : (objectToInt -> Just col)
               : ObjectMap details
               : _
               )) = runMaybeT $ do
  vim_end_col <- hoistMaybe $ objectToInt =<< M.lookup (ObjectString "end_col") details
  -- Plus one here because our lines are 1-indexed but the results of
  -- get_extmarks is 0-indexed.
  let start_line = LineNumber $ fromIntegral $ line + 1
  end_line <- hoistMaybe $ fmap (LineNumber . (+1) . fromIntegral)
            . objectToInt =<< M.lookup (ObjectString "end_row") details
  hlgroup <- hoistMaybe $ objectToText =<< M.lookup (ObjectString "hl_group") details
  sc <- lift $ unvimifyColumn b start_line $ fromIntegral col
  ec <- lift $ unvimifyColumn b end_line   $ fromIntegral vim_end_col
  pure $ ExtmarkStuff
    { es_mark = Extmark $ fromIntegral ext
    , es_hlgroup = hlgroup
    , es_interval =
       Interval { iStart = Pos start_line sc
                , iEnd   = Pos end_line ec
                }
    }
parseExtmark _ _ = pure Nothing


hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure


getExtmarks :: Buffer -> Pos -> Neovim CornelisEnv [ExtmarkStuff]
getExtmarks b p = do
  ns <- asks ce_namespace
  vp <- vimifyPositionM b p
  -- The vim API uses 0-indexed lines for buf_get_extmarks..
  let pos0 = ObjectArray [ ObjectInt $ getVimLineNumber $ p_line vp
                         , ObjectInt 0 -- from the beginning of the line
                         ]
      pos1 = ObjectArray [ ObjectInt $ getVimLineNumber $ p_line vp
                         , ObjectInt (-1) -- to the end of the line
                         ]
  res <- nvim_buf_get_extmarks b ns pos0 pos1 $ M.singleton "details" $ ObjectBool True
  marks <- fmap catMaybes $ traverse (parseExtmark b) $ V.toList res

  pure $ marks >>= \es ->
    case containsPoint (es_interval es) p of
      False -> mempty
      True -> [es]

