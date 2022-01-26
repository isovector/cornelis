{-# LANGUAGE NumDecimals #-}

module Cornelis.Agda where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Aeson
import System.IO
import Control.Monad.IO.Class
import System.Process
import Cornelis.Types.Agda
import Neovim
import Neovim.API.String
import Control.Concurrent (threadDelay)
import Cornelis.Types
import Cornelis.Utils
import Control.Monad (forever)
import Control.Concurrent.Chan.Unagi (writeChan)

spawnAgda :: Buffer -> Neovim CornelisEnv Agda
spawnAgda buffer = do
  (min, mout, _, _) <- liftIO $ createProcess $ (proc "agda" ["--interaction-json"])
      { std_in = CreatePipe , std_out = CreatePipe }
  case (min, mout) of
    (Just hin, Just hout) -> do
      liftIO $ do
        hSetBuffering hin NoBuffering
        hSetBuffering hout NoBuffering

      neovimAsync $ forever $ do
        resp <- liftIO $ hGetLine hout
        chan <- asks ce_stream
        liftIO $ writeChan chan $ AgdaResp buffer resp

      pure $ Agda buffer hin
    (_, _) -> error "can't start agda"


runIOTCM :: Interaction -> Agda -> Neovim env ()
runIOTCM i agda = do
  iotcm <- buildIOTCM i $ a_buffer agda
  vim_err_write $ show iotcm
  liftIO $ hPrint (a_req agda) iotcm


buildIOTCM :: Interaction -> Buffer -> Neovim env IOTCM
buildIOTCM i buffer = do
  fp <- buffer_get_name buffer
  pure $ IOTCM fp None Direct i

withCurrentBuffer :: (Buffer -> Neovim env a) -> Neovim env a
withCurrentBuffer f = vim_get_current_buffer >>= f

