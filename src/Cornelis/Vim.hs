{-# LANGUAGE OverloadedLabels #-}

module Cornelis.Vim where

import Data.Int
import Cornelis.Types
import Cornelis.Types.Agda
import Neovim
import Neovim.API.Text
import Cornelis.Offsets
import Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace (traceM)


vimFirstLine :: Int64
vimFirstLine = 0

vimLastLine :: Int64
vimLastLine = -1


getWindowCursor :: Window -> Neovim env Pos
getWindowCursor w = do
  (row, col) <- window_get_cursor w
  -- window_get_cursor gives us a 1-indexed line, but that is the same way that
  -- lines are indexed.
  let line = LineNumber $ fromIntegral row
  b <- window_get_buffer w
  col' <- unvimifyColumn b line col
  pure $ Pos line col'


getMark :: Buffer -> Char -> Neovim env Pos
getMark b mark = do
  (row, col) <- nvim_buf_get_mark b $ T.singleton mark
  -- buffer_get_mark gives us a 1-indexed line, but that is the same way that
  -- lines are indexed.
  let line = LineNumber $ fromIntegral row
  col' <- unvimifyColumn b line col
  pure $ Pos line col'


setWindowCursor :: Window -> Pos -> Neovim env ()
setWindowCursor w p = do
  b <- window_get_buffer w
  Pos (LineNumber l) c <- vimifyPositionM b p
  -- lines are 1-indexed, but window_set_cursor also wants a 1-index, so we're
  -- cool to not call 'getVimLineNumber' here.
  window_set_cursor w (fromIntegral l, c)

replaceInterval :: Buffer -> Pos -> Pos -> Text -> Neovim env ()
replaceInterval b start end str
  = do
    (sl, sc) <- fmap positionToVim $ vimifyPositionM b start
    (el, ec) <- fmap positionToVim $ vimifyPositionM b end
    nvim_buf_set_text b sl sc el ec $ V.fromList $ T.lines str


------------------------------------------------------------------------------
-- | Vim insists on returning byte-based offsets for the cursor positions...
-- why the fuck? This function undoes the problem.
unvimifyColumn :: Buffer -> LineNumber -> Int64 -> Neovim env LineOffset
unvimifyColumn b l c = do
  lstr <- getBufferLine b l
  pure $ fromBytes lstr $ fromIntegral c

vimifyPositionM :: Buffer -> Pos -> Neovim env (Pos' Int64)
vimifyPositionM b p = do
  l <- getBufferLine b $ p_line p
  pure $ vimifyPosition l p


getBufferLine :: Buffer -> LineNumber -> Neovim env Text
getBufferLine b = buffer_get_line b . getVimLineNumber


vimifyPosition :: Text -> Pos' LineOffset -> Pos' Int64
vimifyPosition t = #p_col %~ fromIntegral . toBytes t


positionToVim :: Pos' Int64 -> (Int64, Int64)
positionToVim p =
  ( fromIntegral $ getVimLineNumber $ p_line p
  , fromIntegral $ p_col p
  )


