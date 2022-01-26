{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Neovim
import Plugin
import Cornelis.Types
import Control.Concurrent (newMVar)
import Control.Concurrent.Async (async)
import Control.Monad.IO.Unlift (withUnliftIO, withRunInIO)
import Control.Concurrent.Chan.Unagi
import Control.Monad (forever)
import Data.Aeson
import Control.Monad.Reader.Class (local)
import Neovim.Context.Internal (Neovim(..), retypeConfig)
import Control.Monad.Trans.Resource (transResourceT)
import Control.Monad.Reader (mapReaderT, withReaderT)
import Neovim.API.String (vim_err_write)
import Cornelis.Utils

main :: IO ()
main = neovim defaultConfig { plugins = [cornelis] }

withLocalEnv :: env -> Neovim env a -> Neovim env' a
withLocalEnv env (Neovim t) = Neovim . flip transResourceT t $ withReaderT (retypeConfig env)

respond :: AgdaResp -> Neovim CornelisEnv ()
respond = vim_err_write . ar_message

cornelis :: Neovim () NeovimPlugin
cornelis = do
  (inchan, outchan) <- liftIO newChan
  mvar <- liftIO $ newMVar $ CornelisState mempty

  let env = CornelisEnv mvar inchan

  vim_err_write "hello from the main thread"
  withLocalEnv env $
    neovimAsync $ do
      vim_err_write "hello from the consumer thread"
      forever $ do
        next <- liftIO $ readChan outchan
        respond next

  wrapPlugin $ Plugin
    { environment = env
    , exports = [ $(command "Cornelis" 'load) [CmdSync Async] ]
    }

