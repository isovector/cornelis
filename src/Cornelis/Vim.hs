{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Cornelis.Vim where

import           Control.Lens
import           Cornelis.Offsets
import           Cornelis.Types
import           Cornelis.Utils (objectToInt, savingCurrentPosition, savingCurrentWindow)
import           Data.Int
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text


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

-- | TODO(sandy): POSSIBLE BUG HERE. MAKE SURE YOU SET THE CURRENT WINDOW
-- BEFORE CALLING THIS FUNCTION
getpos :: Buffer -> Char -> Neovim env Pos
getpos b mark = do
  ObjectArray [_, objectToInt -> Just row, objectToInt -> Just col, _]
    <- vim_call_function "getpos" $ V.fromList [ObjectString $ encodeUtf8 $ T.singleton mark]
  -- getpos gives us a 1-indexed line, but that is the same way that
  -- lines are indexed.
  let line = LineNumber $ fromIntegral row
  col' <- unvimifyColumn b line $ fromIntegral col
  -- but the columns are one indexed!
  pure $ Pos line $ offsetDiff col' $ Offset 1


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


getIndent :: Buffer -> LineNumber -> Neovim env Int
getIndent b l = do
  txt <- getBufferLine b l
  pure $ T.length $ T.takeWhile (== ' ') txt


getBufferLine :: Buffer -> LineNumber -> Neovim env Text
getBufferLine b = buffer_get_line b . getVimLineNumber


vimifyPosition :: Text -> Pos' LineOffset -> Pos' Int64
vimifyPosition t = #p_col %~ fromIntegral . toBytes t


positionToVim :: Pos' Int64 -> (Int64, Int64)
positionToVim p =
  ( fromIntegral $ getVimLineNumber $ p_line p
  , fromIntegral $ p_col p
  )

reportError :: Text -> Neovim env ()
reportError = vim_report_error

reportInfo :: Text -> Neovim env ()
reportInfo m = vim_out_write $ m <> "\n"

------------------------------------------------------------------------------
-- | Awful function that does the motion in visual mode and gives you back
-- where vim thinks the @'<@ and @'>@ marks are.
--
-- I'm so sorry.
getSurroundingMotion
    :: Window
    -> Buffer
    -> Text
    -> Pos
    -> Neovim env (Pos, Pos)
getSurroundingMotion w b motion p = do
  savingCurrentWindow $ do
    savingCurrentPosition w $ do
      nvim_set_current_win w
      setWindowCursor w p
      vim_command $ "normal v" <> motion
      start <- getpos b 'v'
      end <- getpos b '.'
      void $ nvim_input "<esc>"
      pure (start, end)

