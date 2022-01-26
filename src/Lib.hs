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
import Neovim.API.String (vim_err_write, vim_report_error)
import Cornelis.Utils
import Data.ByteString.Lazy.Char8 (unpack)


main :: IO ()
main = neovim defaultConfig { plugins = [cornelis] }


withLocalEnv :: env -> Neovim env a -> Neovim env' a
withLocalEnv env (Neovim t) = Neovim . flip transResourceT t $ withReaderT (retypeConfig env)


respond :: AgdaResp -> Neovim CornelisEnv ()
respond = vim_report_error . show . ar_message


cornelis :: Neovim () NeovimPlugin
cornelis = do
  (inchan, outchan) <- liftIO newChan
  mvar <- liftIO $ newMVar $ CornelisState mempty

  let env = CornelisEnv mvar inchan
  withLocalEnv env $
    neovimAsync $ do
      forever $ do
        next <- liftIO $ readChan outchan
        respond next

  wrapPlugin $ Plugin
    { environment = env
    , exports = pure $ $(command "Cornelis" 'load) [CmdSync Async]
    }

