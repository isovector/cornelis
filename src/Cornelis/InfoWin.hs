module Cornelis.InfoWin (closeInfoWindows, showInfoWindow, buildInfoBuffer) where

import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Cornelis.Types
import Neovim
import Neovim.API.String
import Data.Foldable (for_)
import Data.Maybe
import Data.Traversable (for)
import Control.Monad.State.Class
import Cornelis.Utils (withBufferStuff, windowsForBuffer, savingCurrentWindow, visibleBuffers)
import Cornelis.Pretty
import Prettyprinter (layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.String (renderString)


cornelisWindowVar :: String
cornelisWindowVar = "cornelis_window"

getBufferVariableOfWindow :: NvimObject o => String -> Window -> Neovim env (Maybe o)
getBufferVariableOfWindow variableName window = do
  window_is_valid window >>= \case
    False -> pure Nothing
    True -> do
      buffer <- window_get_buffer window
      let getVariableValue = Just . fromObjectUnsafe <$> buffer_get_var buffer variableName
      getVariableValue `catchNeovimException` const (pure Nothing)

closeInfoWindowsForUnseenBuffers :: Neovim CornelisEnv ()
closeInfoWindowsForUnseenBuffers = do
  seen <- S.fromList <$> visibleBuffers
  bufs <- gets cs_buffers
  let known = M.keysSet bufs
      unseen = known S.\\ seen
  for_ unseen $ \b -> do
    for_ (M.lookup b bufs) $ \bs -> do
      ws <- windowsForBuffer $ iw_buffer $ bs_info_win bs
      for_ ws $ flip nvim_win_close True

closeInfoWindows :: Neovim env ()
closeInfoWindows = do
  ws <- vim_get_windows
  for_ ws $ \w -> getBufferVariableOfWindow cornelisWindowVar w >>= \case
    Just True -> nvim_win_close w True
    _ -> pure ()


closeInfoWindowForBuffer :: BufferStuff -> Neovim CornelisEnv ()
closeInfoWindowForBuffer bs = do
  let InfoBuffer ib = bs_info_win bs
  ws <- windowsForBuffer ib
  for_ ws $ flip nvim_win_close True



showInfoWindow :: Buffer -> Doc HighlightGroup -> Neovim CornelisEnv ()
showInfoWindow b doc = withBufferStuff b $ \bs -> do
  let ib = bs_info_win bs
  closeInfoWindowForBuffer bs
  closeInfoWindowsForUnseenBuffers
  ns <- asks ce_namespace
  writeInfoBuffer ns ib doc
  ws <- windowsForBuffer b
  for_ ws $ buildInfoWindow ib


buildInfoBuffer :: Neovim env InfoBuffer
buildInfoBuffer = do
  b <-
    -- not listed in the buffer list, is throwaway
    nvim_create_buf False True

  -- Setup things in the buffer
  buffer_set_var b cornelisWindowVar $ ObjectBool True
  nvim_buf_set_option b "modifiable" $ ObjectBool False
  pure $ InfoBuffer b




buildInfoWindow :: InfoBuffer -> Window -> Neovim env Window
buildInfoWindow (InfoBuffer split_buf) w = savingCurrentWindow $ do
  nvim_set_current_win w
  vim_command "split"
  split_win <- nvim_get_current_win
  nvim_win_set_buf split_win split_buf

  -- Setup things in the window
  nvim_win_set_option split_win "relativenumber" $ ObjectBool False
  nvim_win_set_option split_win "number" $ ObjectBool False

  size <- nvim_buf_line_count split_buf
  window_set_height split_win size

  pure split_win


writeInfoBuffer :: Int64 -> InfoBuffer -> Doc HighlightGroup -> Neovim env ()
writeInfoBuffer ns iw doc = do
  let sds = layoutPretty defaultLayoutOptions doc
      (hls, sds') = renderWithHlGroups sds
      s = lines $ renderString sds'

  let b = iw_buffer iw
  nvim_buf_set_option b "modifiable" $ ObjectBool True
  buffer_set_lines b 0 (-1) True s

  for_ (concatMap spanInfoHighlights hls) $ \(InfoHighlight (l, sc) ec hg) ->
    nvim_buf_add_highlight
      b ns
      (show hg)
      l sc ec
  nvim_buf_set_option b "modifiable" $ ObjectBool False

