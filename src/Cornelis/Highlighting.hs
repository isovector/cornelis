{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Cornelis.Highlighting where

import           Control.Lens ((<>~))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe
import           Cornelis.Offsets
import           Cornelis.Pretty
import           Cornelis.Types hiding (Type)
import           Cornelis.Utils
import           Cornelis.Vim (unvimify, vimify)
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

highlightBuffer :: Buffer -> [Highlight] -> Neovim CornelisEnv [VimInterval]
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
  { li_intervalMap :: IntervalMap AgdaIndex (LineNumber 'ZeroIndexed, Text)
    -- ^ Mapping from positions to line numbers
  }
  deriving newtype (Semigroup, Monoid)

getLineIntervals :: Vector Text -> LineIntervals
getLineIntervals = LineIntervals . go (toOneIndexed @Int 1) (toZeroIndexed @Int 0)
  where
    go
        :: AgdaIndex
        -> LineNumber 'ZeroIndexed
        -> Vector Text
        -> IntervalMap AgdaIndex (LineNumber 'ZeroIndexed, Text)
    go pos line v
      | Just (t, ss) <- V.uncons v =
        let len = T.length t
            pos' = pos .+ Offset len
        in IM.insert (IM.Interval pos pos') (line, t)
              $ go (incIndex pos') (incIndex line) ss
      | otherwise = mempty

lookupPoint :: LineIntervals -> AgdaIndex -> Maybe VimPos
lookupPoint (LineIntervals im) i = do
  (IM.Interval lineStart _, (line, s)) <- listToMaybe $ IM.search i im
  let col = toBytes s (toZeroIndexed @Int 0 .+ (i .-. lineStart))
  pure (Pos line col)

------------------------------------------------------------------------------
-- | Returns any holes it tried to highlight on the left
addHighlight
    :: Buffer
    -> LineIntervals
    -> Highlight
    -> Neovim CornelisEnv ([VimInterval], Maybe Extmark)
addHighlight b lis hl = do
  case Interval
           <$> lookupPoint lis (hl_start hl)
           <*> lookupPoint lis (hl_end hl) of
    Just (int@(Interval start end)) -> do
      let atom = fromMaybe "" $ listToMaybe $ hl_atoms hl
      ext <- setHighlight b int $ hlGroup atom

      pure $ (, ext) $ case atom == "hole" of
        False -> []
        True ->
          pure $ Interval start (end `addCol` Offset 1)
    Nothing -> pure ([], Nothing)


setHighlight
    :: Buffer
    -> Interval VimPos
    -> HighlightGroup
    -> Neovim CornelisEnv (Maybe Extmark)
setHighlight b (Interval (Pos sl sc) (Pos el ec)) hl = do
  ns <- asks ce_namespace
  let from0 = fromZeroIndexed
  flip catchNeovimException (const (pure Nothing))
    $ fmap (Just . coerce) $ nvim_buf_set_extmark b ns (from0 sl) (from0 sc) $ M.fromList
    [ ( "end_line"
      , ObjectInt (from0 el)
      )
    , ( "end_col"
      , ObjectInt $ from0 ec
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
    -> AgdaInterval
    -> HighlightGroup
    -> Neovim CornelisEnv (Maybe Extmark)
highlightInterval b int hl = do
  int' <- traverse (vimify b) int
  setHighlight b int' hl


parseExtmark :: Buffer -> Object -> Neovim CornelisEnv (Maybe ExtmarkStuff)
parseExtmark b
  (ObjectArray ( (objectToInt -> Just ext)
               : (objectToInt @Int -> Just (toZeroIndexed -> start_line))
               : (objectToInt @Int -> Just (toZeroIndexed -> start_col))
               : ObjectMap details
               : _
               )) = runMaybeT $ do
  end_col <- hoistMaybe $ fmap toZeroIndexed
            . objectToInt @Int =<< M.lookup (ObjectString "end_col") details
  end_line <- hoistMaybe $ fmap toZeroIndexed
            . objectToInt @Int =<< M.lookup (ObjectString "end_row") details
  hlgroup <- hoistMaybe $ objectToText =<< M.lookup (ObjectString "hl_group") details
  int <- lift $ traverse (unvimify b) (Interval (Pos start_line start_col) (Pos end_line end_col))
  pure $ ExtmarkStuff
    { es_mark = Extmark ext
    , es_hlgroup = hlgroup
    , es_interval = int
    }
parseExtmark _ _ = pure Nothing


hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure


getExtmarks :: Buffer -> AgdaPos -> Neovim CornelisEnv [ExtmarkStuff]
getExtmarks b p = do
  ns <- asks ce_namespace
  vp <- vimify b p
  let -- i = 0 for beginning of line, i = -1 for end of line
      pos i = ObjectArray [ ObjectInt $ fromZeroIndexed (p_line vp)
                          , ObjectInt i
                          ]
  res <- nvim_buf_get_extmarks b ns (pos 0) (pos (-1)) $ M.singleton "details" $ ObjectBool True
  marks <- fmap catMaybes $ traverse (parseExtmark b) $ V.toList res

  pure $ marks >>= \es ->
    case containsPoint (es_interval es) p of
      False -> mempty
      True -> [es]

