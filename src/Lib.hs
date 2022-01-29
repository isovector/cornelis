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
import Neovim.API.String
import Cornelis.Utils
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Monad.State.Class (modify', gets)
import qualified Data.IntMap.Strict as IM
import Control.Arrow ((&&&), first)
import Data.Foldable (for_)
import Cornelis.Types.Agda
import qualified Data.Map.Strict as M
import Cornelis.Highlighting (highlightBuffer, getLineIntervals, lookupPoint)
import Data.List (intercalate)
import Cornelis.InfoWin
import Control.Monad (when)
import Data.Maybe


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
  when (dp & hasn't #_GoalSpecific) $
    modifyBufferStuff b $ #bs_goals .~ dp
  goalWindow b dp
-- Update the buffer's interaction points map
respond b (InteractionPoints ips) = do
  modifyBufferStuff b $ #bs_ips .~ (IM.fromList $ fmap (ip_id &&& id) ips)
-- Replace a function clause
respond b (MakeCase mkcase) = do
  doMakeCase b mkcase
-- Replace the interaction point with a result
respond b (GiveAction result ip) = do
  replaceInterval b (ip_interval ip) result
-- Replace the interaction point with a result
respond b (SolveAll solutions) = do
  for_ solutions $ \(Solution i ex) -> do
    getInteractionPoint b i >>= \case
      Nothing -> vim_report_error $ "Can't find interaction point " <> show i
      Just ip -> replaceInterval b (ip_interval ip) $ parens ex
respond b ClearHighlighting = do
  ns <- asks ce_namespace
  nvim_buf_clear_namespace b ns 0 (-1)
respond b (HighlightingInfo _remove hl) =
  highlightBuffer b hl
respond _ (RunningInfo _ x) = vim_out_write x
respond _ (ClearRunningInfo) = vim_out_write ""
respond b (JumpToError _ pos) = do
  buf_lines <- nvim_buf_get_lines b 0 (-1) True
  let li = getLineIntervals buf_lines
  case lookupPoint li pos of
    Nothing -> vim_report_error "invalid error report from Agda"
    Just lc -> do
      ws <- windowsForBuffer b
      for_ ws $ flip window_set_cursor $ first (+1) lc
respond _ (Unknown k _) = vim_report_error k
respond _ x = pure ()

parens :: String -> String
parens s = '(' : s <> ")"

------------------------------------------------------------------------------
-- | Awful function that does the motion in visual mode and gives you back
-- where vim thinks the @'<@ and @'>@ marks are.
--
-- I'm so sorry.
getSurroundingMotion
    :: Window
    -> Buffer
    -> String
    -> Position' a
    -> Neovim CornelisEnv ((Int64, Int64), (Int64, Int64))
getSurroundingMotion w b motion p = do
  savingCurrentWindow $ do
    savingCurrentPosition w $ do
      let lc = positionToVim p
      nvim_set_current_win w
      window_set_cursor w lc
      vim_command $ "normal v" <> motion
      start <- nvim_buf_get_mark b "<"
      end <- nvim_buf_get_mark b ">"
      nvim_input "<esc>"
      pure (start, end)

doMakeCase :: Buffer -> MakeCase -> Neovim CornelisEnv ()
doMakeCase b (RegularCase Function clauses ip) =
  replaceInterval b (ip_interval ip & #iStart . #posCol .~ 1) $ unlines clauses
-- TODO(sandy): It would be nice if Agda just gave us the bounds we're supposed to replace...
doMakeCase b (RegularCase ExtendedLambda clauses ip) = do
  ws <- windowsForBuffer b
  case listToMaybe ws of
    Nothing ->
      vim_report_error
        "Unable to extend a lambda without having a window that contains the modified buffer. This is a bug in cornelis."
    Just w -> do
      ((sl, sc), (el, ec)) <- getSurroundingMotion w b "i}" $ iStart $ ip_interval ip
      nvim_buf_set_text b (sl - 1) (sc + 1) (el - 1) ec $
        clauses & _tail %~ fmap (indent $ fromIntegral sc)

------------------------------------------------------------------------------
-- | Indent a string with the given offset.
indent :: Int -> String -> String
indent n s = replicate (n - 1) ' ' <> "; " <> s


positionToVim :: Position' a -> (Int64, Int64)
positionToVim p =
  ( fromIntegral $ posLine p - 1
  , fromIntegral $ posCol p - 1
  )



replaceInterval :: Buffer -> IntervalWithoutFile -> String -> Neovim CornelisEnv ()
replaceInterval buffer (Interval (positionToVim -> (sl, sc)) (positionToVim -> (el, ec)))
  = nvim_buf_set_text buffer sl sc el ec
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
        , $(command "CornelisRefine" 'refine) [CmdSync Async]
        ]
    }

