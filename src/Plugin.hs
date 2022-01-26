{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}

module Plugin where

import Control.Lens
import qualified Data.Map as M
import Data.Map (Map)
import Neovim
import Neovim.API.String
import Data.Foldable (for_)
import Cornelis.Types
import Neovim.Context (gets)
import Control.Monad.State.Class
import Data.Generics.Labels


withAgda :: Neovim CornelisEnv a -> Neovim CornelisEnv a
withAgda m = do
  buffer <- vim_get_current_buffer
  gets (M.lookup buffer . cs_procs) >>= \case
    Just _ -> m
    Nothing -> do
      agda <- spawnAgda
      modify' $ #cs_procs %~ M.insert buffer agda
      m

spawnAgda :: Neovim CornelisEnv (IO ())
spawnAgda = error "not implemented"

yo :: String -> Neovim env ()
yo str = do
  buffers <- vim_get_buffers
  for_ buffers $ \buffer -> do
    buffer_set_line buffer 0 str

