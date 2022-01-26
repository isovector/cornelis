{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Agda where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Aeson
import System.IO hiding (hGetLine)
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
import Data.List (isPrefixOf)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Foldable (for_)
import Data.Text.Lazy.IO (hGetLine)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

spawnAgda :: Buffer -> Neovim CornelisEnv Agda
spawnAgda buffer = do
  (min, mout, _, _) <-
    liftIO $ createProcess $
      (proc "agda" ["--interaction-json"])
        { std_in = CreatePipe , std_out = CreatePipe }
  case (min, mout) of
    (Just hin, Just hout) -> do
      liftIO $ do
        hSetBuffering hin NoBuffering
        hSetBuffering hout NoBuffering

      neovimAsync $ forever $ do
        resp <- liftIO $ hGetLine hout
        chan <- asks ce_stream
        case eitherDecode @Response $ encodeUtf8 $ (dropPrefix "JSON> ") resp of
          Left err -> vim_report_error err
          Right re -> liftIO $ writeChan chan $ AgdaResp buffer re

      pure $ Agda buffer hin
    (_, _) -> error "can't start agda"


dropPrefix :: Text -> Text -> Text
dropPrefix pref msg
  | T.isPrefixOf pref msg = T.drop (T.length pref) msg
  | otherwise = msg


runIOTCM :: Interaction -> Agda -> Neovim env ()
runIOTCM i agda = do
  iotcm <- buildIOTCM i $ a_buffer agda
  liftIO $ hPrint (a_req agda) iotcm


buildIOTCM :: Interaction -> Buffer -> Neovim env IOTCM
buildIOTCM i buffer = do
  fp <- buffer_get_name buffer
  pure $ IOTCM fp NonInteractive Direct i

withCurrentBuffer :: (Buffer -> Neovim env a) -> Neovim env a
withCurrentBuffer f = vim_get_current_buffer >>= f

