{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Cornelis.Vim where

import           Control.Lens ((%~), _head, _last, (&))
import           Cornelis.Offsets
import           Cornelis.Types
import           Cornelis.Utils (objectToInt, savingCurrentPosition, savingCurrentWindow)
import           Data.Foldable (toList)
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
  (oneIndexed -> row, zeroIndexed -> col) <- window_get_cursor w
  -- window_get_cursor gives us a 1-indexed line, but that is the same way that
  -- lines are indexed.
  let line = zeroIndex row
  b <- window_get_buffer w
  unvimify b (TextPos line col)

-- | TODO(sandy): POSSIBLE BUG HERE. MAKE SURE YOU SET THE CURRENT WINDOW
-- BEFORE CALLING THIS FUNCTION
getpos :: Buffer -> Char -> Neovim env Pos
getpos b mark = do
  -- getpos gives us a (1,1)-indexed position!
  ObjectArray [_, objectToInt @Int -> Just (oneIndexed -> line), objectToInt @Int -> Just (oneIndexed -> col), _]
    <- vim_call_function "getpos" $ V.fromList [ObjectString $ encodeUtf8 $ T.singleton mark]
  unvimify b (TextPos (zeroIndex line) (zeroIndex col))

data SearchMode = Forward | Backward
  deriving (Eq, Ord, Show)

searchpos :: Buffer -> [Text] -> SearchMode -> Neovim env Pos
searchpos b pats dir = do
  -- unlike getpos, these columns are 0 indexed W T F
  ObjectArray [objectToInt @Int -> Just (oneIndexed -> line), objectToInt @Int -> Just (zeroIndexed -> col)]
    <- vim_call_function "searchpos" $ V.fromList
        [ ObjectString $ encodeUtf8 $ T.intercalate "\\|" pats
        , ObjectString $ encodeUtf8 $ case dir of
            Forward -> "n"
            Backward -> "bn"
        ]
  unvimify b (TextPos (zeroIndex line) col)

setWindowCursor :: Window -> Pos -> Neovim env ()
setWindowCursor w p = do
  b <- window_get_buffer w
  TextPos l c <- vimify b p
  window_set_cursor w (fromOneIndexed (oneIndex l), fromZeroIndexed c)

replaceInterval :: Buffer -> Interval Pos -> Text -> Neovim env ()
replaceInterval b ival str
  = do
    Interval (TextPos sl sc) (TextPos el ec) <- traverse (vimify b) ival
    nvim_buf_set_text b (zi sl) (zi sc) (zi el) (zi ec) $ V.fromList $ T.lines str
  where
    zi = fromZeroIndexed

------------------------------------------------------------------------------
-- | Vim insists on returning byte-based offsets for the cursor positions...
-- why the fuck? This function undoes the problem.
unvimify :: Buffer -> VimPos -> Neovim env Pos
unvimify b (TextPos line col) = do
  txt <- getBufferLine b line
  let col' = fromBytes txt col
  pure (TextPos line col')

vimify :: Buffer -> Pos -> Neovim env VimPos
vimify b (TextPos line col) = do
  txt <- getBufferLine b line
  let col' = toBytes txt col
  pure (TextPos line col')

getIndent :: Buffer -> LineNumber 'ZeroIndexed -> Neovim env Int
getIndent b l = do
  txt <- getBufferLine b l
  pure $ T.length $ T.takeWhile (== ' ') txt


getBufferLine :: Buffer -> LineNumber 'ZeroIndexed -> Neovim env Text
getBufferLine b l = buffer_get_line b (fromZeroIndexed l)

getBufferInterval :: Buffer -> Interval Pos -> Neovim env Text
getBufferInterval b (Interval start end) = do
    TextPos sl _ <- vimify b start
    TextPos el _ <- vimify b end
    -- nvim_buf_get_lines is exclusive in its end line, thus the plus 1
    ls <- fmap toList $ nvim_buf_get_lines b (zi sl) (zi el + 1) False
    pure $ T.unlines $
      ls & _last %~ T.take (zi (p_col end))
         & _head %~ T.drop (zi (p_col start))
  where
    zi i = fromZeroIndexed i

reportError :: Text -> Neovim env ()
reportError = vim_report_error

reportInfo :: Text -> Neovim env ()
reportInfo m = vim_out_write $ m <> "\n"

setreg :: Text -> Text -> Neovim env ()
setreg reg val
  = void
  $ vim_call_function "setreg"
  $ V.fromList
    [ ObjectString $ encodeUtf8 reg
    , ObjectString $ encodeUtf8 val
    ]

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

------------------------------------------------------------------------------
-- | Get an interval to replace for a lambda case split
getLambdaClause
    :: Window
    -> Buffer
    -> Pos -- ^ Start of IP interval
    -> Pos -- ^ End of IP interval
    -> Neovim env (Pos, Pos)
getLambdaClause w b p0 p1 = do
  savingCurrentWindow $ do
    savingCurrentPosition w $ do
      nvim_set_current_win w
      setWindowCursor w p0
      start <- searchpos b ["{", ";"] Backward
      setWindowCursor w p1
      end <- searchpos b [";", "}"] Forward
      pure (start, end)

