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
import Cornelis.Agda (spawnAgda, withCurrentBuffer, runIOTCM)
import Cornelis.Types.Agda


withAgda :: Neovim CornelisEnv a -> Neovim CornelisEnv a
withAgda m = do
  buffer <- vim_get_current_buffer
  gets (M.lookup buffer . cs_procs) >>= \case
    Just _ -> m
    Nothing -> do
      agda <- spawnAgda buffer
      modify' $ #cs_procs %~ M.insert buffer agda
      m

getAgda :: Buffer -> Neovim CornelisEnv Agda
getAgda buffer = gets $ (M.! buffer) . cs_procs

load :: CommandArguments -> Neovim CornelisEnv ()
load _ = withAgda $ do
  agda <- withCurrentBuffer getAgda
  name <- buffer_get_name $ a_buffer agda
  flip runIOTCM agda $ Cmd_load name []

