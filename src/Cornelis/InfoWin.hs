module Cornelis.InfoWin (closeInfoWindows, showInfoWindow, buildInfoBuffer) where

import qualified Data.Map as M
import Data.Map (Map)
import Cornelis.Types
import Neovim
import Neovim.API.String
import Data.Foldable (for_)
import Data.Maybe
import Data.Traversable (for)
import Control.Monad.State.Class
import Cornelis.Utils (withBufferStuff, windowsForBuffer, savingCurrentWindow)


cornelisWindowVar :: String
cornelisWindowVar = "cornelis_window"


closeInfoWindows :: Neovim env ()
closeInfoWindows = do
  ws <- vim_get_windows
  for_ ws $ \w ->
    window_is_valid w >>= \case
      False -> pure ()
      True -> do
        b <- window_get_buffer w
        buffer_get_var b cornelisWindowVar >>= \case
          ObjectBool True -> do
            nvim_win_close w True
          _ -> pure ()


closeInfoWindowForBuffer :: BufferStuff -> Neovim CornelisEnv ()
closeInfoWindowForBuffer bs = do
  let InfoBuffer ib = bs_info_win bs
  ws <- windowsForBuffer ib
  for_ ws $ flip nvim_win_close True



showInfoWindow :: Buffer -> [String] -> Neovim CornelisEnv ()
showInfoWindow b s = withBufferStuff b $ \bs -> do
  let ib = bs_info_win bs
  closeInfoWindowForBuffer bs
  writeInfoBuffer ib s
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
  buffer_set_var split_buf cornelisWindowVar $ ObjectBool True
  nvim_win_set_option split_win "relativenumber" $ ObjectBool False
  nvim_win_set_option split_win "number" $ ObjectBool False

  size <- nvim_buf_line_count split_buf
  window_set_height split_win size

  pure split_win


writeInfoBuffer :: InfoBuffer -> [String] -> Neovim env ()
writeInfoBuffer iw s = do
  let b = iw_buffer iw
  nvim_buf_set_option b "modifiable" $ ObjectBool True
  buffer_set_lines b 0 (-1) True s
  nvim_buf_set_option b "modifiable" $ ObjectBool False

