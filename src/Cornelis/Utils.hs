module Cornelis.Utils where

import Neovim
import Control.Concurrent.Async
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))


neovimAsync :: (MonadUnliftIO m) => m a -> m (Async a)
neovimAsync m =
  withRunInIO $ \lower ->
    liftIO $ async $ lower m

