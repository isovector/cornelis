{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Utils where

import           Control.Concurrent.Async
import           Control.Exception (throwIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import           Control.Monad.State.Class
import           Cornelis.Types
import qualified Data.Map as M
import           Data.Maybe
import           Data.Traversable
import qualified Data.Vector as V
import           Neovim hiding (err)
import           Neovim.API.Text


neovimAsync :: (MonadUnliftIO m) => m a -> m (Async a)
neovimAsync m =
  withRunInIO $ \lower ->
    liftIO $ async $ lower m

withBufferStuff :: Buffer -> (BufferStuff -> Neovim CornelisEnv ()) -> Neovim CornelisEnv ()
withBufferStuff b f =
  gets (M.lookup b . cs_buffers) >>= \case
    Nothing -> vim_report_error "no buffer stuff!"
    Just bs -> f bs

savingCurrentPosition :: Window -> Neovim env a -> Neovim env a
savingCurrentPosition w m = do
  c <- window_get_cursor w
  m <* window_set_cursor w c

savingCurrentWindow :: Neovim env a -> Neovim env a
savingCurrentWindow m = do
  w <- nvim_get_current_win
  m <* nvim_set_current_win w

windowsForBuffer :: Buffer -> Neovim env [Window]
windowsForBuffer b = do
  wins <- fmap V.toList $ vim_get_windows
  fmap catMaybes $ for wins $ \w -> do
    wb <- window_get_buffer w
    pure $ case wb == b of
      False -> Nothing
      True -> Just w

visibleBuffers :: Neovim env [Buffer]
visibleBuffers = do
  wins <- fmap V.toList $ vim_get_windows
  for wins window_get_buffer

criticalFailure :: Text -> Neovim env a
criticalFailure err = do
  vim_report_error err
  liftIO $ throwIO $ ErrorResult "critical error" ObjectNil

