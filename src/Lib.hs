{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Lib where

import Neovim
import Plugin
import Control.Lens
import Cornelis.Types
import Control.Concurrent (newMVar, threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad.IO.Unlift (withUnliftIO, withRunInIO)
import Control.Concurrent.Chan.Unagi
import Control.Monad (forever)
import Data.Aeson
import Control.Monad.Reader.Class (local)
import Neovim.Context.Internal (Neovim(..), retypeConfig)
import Control.Monad.Trans.Resource (transResourceT)
import Control.Monad.Reader (mapReaderT, withReaderT)
import Neovim.API.String (vim_err_write, vim_report_error, Buffer, nvim_buf_set_text, nvim_create_namespace, nvim_buf_clear_namespace, vim_command_output, vim_out_write, nvim_open_win, nvim_get_current_win, vim_command, nvim_create_buf, nvim_win_set_buf, buffer_set_lines, window_set_height, nvim_set_current_win, nvim_win_set_var, vim_get_windows, nvim_win_get_var, nvim_win_close)
import Cornelis.Utils
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Monad.State.Class (modify', gets)
import qualified Data.IntMap.Strict as IM
import Control.Arrow ((&&&))
import Data.Foldable (for_)
import Cornelis.Types.Agda
import qualified Data.Map.Strict as M
import Cornelis.Highlighting (highlightBuffer)
import Data.List (intercalate)
import Cornelis.InfoWin


main :: IO ()
main = neovim defaultConfig { plugins = [cornelis] }


withLocalEnv :: env -> Neovim env a -> Neovim env' a
withLocalEnv env (Neovim t) = Neovim . flip transResourceT t $ withReaderT (retypeConfig env)


getInteractionPoint :: Buffer -> Int -> Neovim CornelisEnv (Maybe InteractionPoint)
getInteractionPoint b i = gets $ preview $ #cs_buffers . ix b . #bs_ips . ix i

modifyBufferStuff :: Buffer -> (BufferStuff -> BufferStuff) -> Neovim CornelisEnv ()
modifyBufferStuff b f = modify' $ #cs_buffers %~ M.update (Just . f) b


respond :: Buffer -> Response -> Neovim CornelisEnv ()
-- Update the buffer's goal map
respond b (DisplayInfo dp) = do
  modifyBufferStuff b $ #bs_goals .~ dp
  goalWindow b dp
-- Update the buffer's interaction points map
respond b (InteractionPoints ips) = do
  modifyBufferStuff b $ #bs_ips .~ (IM.fromList $ fmap (ip_id &&& id) ips)
-- Replace a function clause
respond b (MakeCase (MakeFunctionCase clauses ip)) = do
  replaceInterval b (ip_interval ip & #iStart . #posCol .~ 1) $ unlines clauses
-- Replace the interaction point with a result
respond b (SolveAll solutions) = do
  for_ solutions $ \(Solution i ex) -> do
    getInteractionPoint b i >>= \case
      Nothing -> vim_report_error $ "Can't find interaction point " <> show i
      Just ip -> replaceInterval b (ip_interval ip) ex
respond b ClearHighlighting = do
  ns <- asks ce_namespace
  nvim_buf_clear_namespace b ns 0 (-1)
respond b (HighlightingInfo _remove hl) =
  highlightBuffer b hl
respond _ (RunningInfo _ x) = vim_out_write x
respond _ (Unknown k _) = vim_report_error k
respond _ x = pure ()


replaceInterval :: Buffer -> IntervalWithoutFile -> String -> Neovim CornelisEnv ()
replaceInterval buffer (Interval start end)
  = nvim_buf_set_text
      buffer
      (fromIntegral $ posLine start - 1)
      (fromIntegral $ posCol start - 1)
      (fromIntegral $ posLine end - 1)
      (fromIntegral $ posCol end - 1)
  . lines


cornelis :: Neovim () NeovimPlugin
cornelis = do
  (inchan, outchan) <- liftIO newChan
  ns <- nvim_create_namespace "cornelis"
  mvar <- liftIO $ newMVar $ CornelisState mempty

  let env = CornelisEnv mvar inchan ns
  withLocalEnv env $
    neovimAsync $ do
      forever $ do
        AgdaResp buffer next <- liftIO $ readChan outchan
        respond buffer next

  -- https://github.com/neovimhaskell/nvim-hs/issues/94
  neovimAsync $ do
    liftIO $ threadDelay 1e6
    closeInfoWindows

  wrapPlugin $ Plugin
    { environment = env
    , exports =
        [ $(command "CornelisLoad" 'load) [CmdSync Async]
        , $(command "CornelisGoals" 'allGoals) [CmdSync Async]
        , $(command "CornelisSolve" 'solveOne) [CmdSync Async]
        , $(command "CornelisTypeContext" 'typeContext) [CmdSync Async]
        , $(command "CornelisMakeCase" 'caseSplit) [CmdSync Async]
        ]
    }

