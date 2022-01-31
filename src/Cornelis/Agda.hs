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
import Neovim.API.Text
import Control.Concurrent (threadDelay)
import Cornelis.Types
import Cornelis.Utils
import Control.Monad (forever, when)
import Control.Concurrent.Chan.Unagi (writeChan)
import Data.List (isPrefixOf)
import Data.Foldable (for_)
import Data.Text.Lazy.IO (hGetLine)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Bool (bool)
import Cornelis.Debug (reportExceptions)
import qualified Data.Text as T

debugJson :: Bool
debugJson = False

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

      neovimAsync $ forever $ reportExceptions $ do
        resp <- liftIO $ hGetLine hout
        chan <- asks ce_stream
        when debugJson $ vim_report_error $ T.pack $ show resp
        case eitherDecode @Response $ encodeUtf8 $ (dropPrefix "JSON> ") resp of
          Left err -> vim_report_error $ T.pack err
          Right re -> liftIO $ writeChan chan $ AgdaResp buffer re

      pure $ Agda buffer hin
    (_, _) -> error "can't start agda"


dropPrefix :: LT.Text -> LT.Text -> LT.Text
dropPrefix pref msg
  | LT.isPrefixOf pref msg = LT.drop (LT.length pref) msg
  | otherwise = msg


runIOTCM :: Interaction -> Agda -> Neovim env ()
runIOTCM i agda = do
  iotcm <- buildIOTCM i $ a_buffer agda
  liftIO $ hPrint (a_req agda) iotcm


enableHighlighting :: HighlightingLevel
enableHighlighting = bool NonInteractive None debugJson

buildIOTCM :: Interaction -> Buffer -> Neovim env IOTCM
buildIOTCM i buffer = do
  fp <- buffer_get_name buffer
  pure $ IOTCM fp enableHighlighting Direct i

withCurrentBuffer :: (Buffer -> Neovim env a) -> Neovim env a
withCurrentBuffer f = vim_get_current_buffer >>= f

