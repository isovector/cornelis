module Cornelis.Utils where

import qualified Data.Map as M
import Data.Map (Map)
import Neovim
import Control.Concurrent.Async
import Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import Cornelis.Types
import Control.Monad.State.Class
import Neovim.API.String


neovimAsync :: (MonadUnliftIO m) => m a -> m (Async a)
neovimAsync m =
  withRunInIO $ \lower ->
    liftIO $ async $ lower m

withBufferStuff :: Buffer -> (BufferStuff -> Neovim CornelisEnv ()) -> Neovim CornelisEnv ()
withBufferStuff b f =
  gets (M.lookup b . cs_buffers) >>= \case
    Nothing -> vim_report_error "no buffer stuff!"
    Just bs -> f bs

