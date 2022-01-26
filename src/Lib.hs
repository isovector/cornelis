{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Lib where

import Neovim
import Plugin
import Control.Lens
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
import Neovim.API.String (vim_err_write, vim_report_error, Buffer, nvim_buf_set_text, nvim_create_namespace, nvim_buf_clear_namespace, vim_command_output, vim_out_write, nvim_open_win, nvim_get_current_win, vim_command, nvim_create_buf, nvim_win_set_buf, buffer_set_lines, window_set_height, nvim_set_current_win)
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


main :: IO ()
main = neovim defaultConfig { plugins = [cornelis] }


withLocalEnv :: env -> Neovim env a -> Neovim env' a
withLocalEnv env (Neovim t) = Neovim . flip transResourceT t $ withReaderT (retypeConfig env)


getInteractionPoint :: Buffer -> Int -> Neovim CornelisEnv (Maybe InteractionPoint)
getInteractionPoint b i = gets $ preview $ #cs_ips . ix b . ix i

respond :: Buffer -> Response -> Neovim CornelisEnv ()
-- Update the buffer's goal map
respond b (DisplayInfo dp) = do
  modify' $ #cs_goals %~ M.insert b dp
  goalWindow b
-- Update the buffer's interaction points map
respond b (InteractionPoints ips) = do
  modify' $ #cs_ips %~ M.insert b (IM.fromList $ fmap (ip_id &&& id) ips)
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

goalWindow :: Buffer -> Neovim CornelisEnv ()
goalWindow b = do
  -- TODO(sandy): bug -- find the window that corresponds to this buffer!!
  buffer_win <- nvim_get_current_win
  mgoals <- gets $ M.lookup b . cs_goals
  for_ mgoals $ \goals -> do
    vim_command "split"
    split_win <- nvim_get_current_win
    split_buf <-
      -- not listed in the buffer list, is throwaway
      nvim_create_buf False True
    nvim_win_set_buf split_win split_buf
    let contents = showGoals goals
    buffer_set_lines split_buf 0 (-1) True contents
    window_set_height split_win $ fromIntegral $ length contents

    vim_command "set norelativenumber"
    vim_command "set nonumber"
    vim_command "set nomodifiable"

    -- restore window
    nvim_set_current_win buffer_win

showGoals :: DisplayInfo -> [String]
showGoals (AllGoalsWarnings vis invis) = lines $ intercalate "\n" $ concat
  [ [ unlines
      [ "Visible Goals:"
      , unlines $ fmap showGoal vis
      ]
    | not $ null vis
    ]
  , [ unlines
      [ "Invisible Goals:"
      , unlines $ fmap showGoal vis
      ]
    | not $ null invis
    ]
  ]
showGoals (UnknownDisplayInfo _) = []

showGoal :: GoalInfo -> String
showGoal (GoalInfo ip ty) = unwords
  [ "?" <> show (ip_id ip)
  , " : "
  , ty
  ]


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
  mvar <- liftIO $ newMVar $ CornelisState mempty mempty mempty

  let env = CornelisEnv mvar inchan ns
  withLocalEnv env $
    neovimAsync $ do
      forever $ do
        AgdaResp buffer next <- liftIO $ readChan outchan
        respond buffer next

  wrapPlugin $ Plugin
    { environment = env
    , exports =
        [ $(command "CornelisLoad" 'load) [CmdSync Async]
        , $(command "CornelisSolve" 'solveOne) [CmdSync Async]
        , $(command "CornelisMakeCase" 'caseSplit) [CmdSync Async]
        ]
    }

