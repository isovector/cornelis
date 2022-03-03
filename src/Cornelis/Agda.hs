{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Agda where

import           Control.Concurrent.Chan.Unagi (writeChan)
import           Control.Monad (forever, when)
import           Control.Monad.IO.Class
import           Cornelis.Debug (reportExceptions)
import           Cornelis.Types
import           Cornelis.Types.Agda
import           Cornelis.Utils
import           Data.Aeson
import           Data.Bool (bool)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.Lazy.IO (hGetLine)
import           Neovim hiding (err)
import           Neovim.API.Text
import           System.IO hiding (hGetLine)
import           System.Process


debugJson :: Bool
debugJson = False

spawnAgda :: Buffer -> Neovim CornelisEnv Agda
spawnAgda buffer = do
  (m_in, m_out, _, hdl) <-
    liftIO $ createProcess $
      (proc "agda" ["--interaction-json"])
        { std_in = CreatePipe , std_out = CreatePipe }
  case (m_in, m_out) of
    (Just hin, Just hout) -> do
      liftIO $ do
        hSetBuffering hin NoBuffering
        hSetBuffering hout NoBuffering

      void $ neovimAsync $ forever $ reportExceptions $ do
        resp <- liftIO $ hGetLine hout
        chan <- asks ce_stream
        when debugJson $ vim_report_error $ T.pack $ show resp
        case eitherDecode @Response $ encodeUtf8 $ (dropPrefix "JSON> ") resp of
          Left err -> vim_report_error $ T.pack err
          Right re -> liftIO $ writeChan chan $ AgdaResp buffer re

      pure $ Agda buffer hin hdl
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

