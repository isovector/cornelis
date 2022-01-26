module Cornelis.InfoWin where
import Cornelis.Types
import Neovim
import Neovim.API.String
import Data.Foldable (for_)
import Data.Maybe
import Data.Traversable (for)


cornelisWindowVar :: String
cornelisWindowVar = "cornelis_window"


closeInfoWindows :: Neovim env ()
closeInfoWindows = do
  ws <- vim_get_windows
  for_ ws $ \w ->
    window_is_valid w >>= \case
      False -> pure ()
      True -> do
        nvim_win_get_var w cornelisWindowVar >>= \case
          ObjectBool True -> do
            nvim_win_close w True
          _ -> pure ()


windowsForBuffer :: Buffer -> Neovim env [Window]
windowsForBuffer b = do
  wins <- vim_get_windows
  fmap catMaybes $ for wins $ \w -> do
    wb <- window_get_buffer w
    pure $ case wb == b of
      False -> Nothing
      True -> Just w


buildInfoWindow :: Window -> Neovim env InfoWin
buildInfoWindow w = do
  vim_command "split"
  split_win <- nvim_get_current_win
  split_buf <-
    -- not listed in the buffer list, is throwaway
    nvim_create_buf False True
  nvim_win_set_buf split_win split_buf

  -- Setup things in the window
  nvim_win_set_var split_win cornelisWindowVar $ ObjectBool True
  nvim_win_set_option split_win "relativenumber" $ ObjectBool False
  nvim_win_set_option split_win "number" $ ObjectBool False
  nvim_buf_set_option split_buf "modifiable" $ ObjectBool False

  -- restore window
  nvim_set_current_win w
  pure $ InfoWin split_win split_buf


writeInfoWindow :: InfoWin -> [String] -> Neovim env ()
writeInfoWindow iw s = do
  let b = iw_buffer iw
  nvim_buf_set_option b "modifiable" $ ObjectBool True
  buffer_set_lines b 0 (-1) True s
  window_set_height (iw_window iw) $ fromIntegral $ length s
  nvim_buf_set_option b "modifiable" $ ObjectBool False

