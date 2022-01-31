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
import Neovim.API.Text
import Cornelis.Utils
import Control.Monad.State.Class (modify', gets)
import qualified Data.IntMap.Strict as IM
import Control.Arrow ((&&&), first)
import Data.Foldable (for_)
import Cornelis.Types.Agda
import qualified Data.Map.Strict as M
import Cornelis.Highlighting (highlightBuffer, getLineIntervals, lookupPoint, unvimifyColumn)
import Data.List (intercalate)
import Cornelis.InfoWin
import Control.Monad (when)
import Data.Maybe
import Data.Text (Text)
import Cornelis.Offsets
import qualified Data.Text as T
import Cornelis.Debug (reportExceptions)
import qualified Data.Vector as V


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
      Nothing -> vim_report_error $ T.pack $ "Can't find interaction point " <> show i
      Just ip -> do
        replaceInterval b (ip_interval ip) $ parens ex
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

parens :: Text -> Text
parens s = "(" <> s <> ")"

------------------------------------------------------------------------------
-- | Awful function that does the motion in visual mode and gives you back
-- where vim thinks the @'<@ and @'>@ marks are.
--
-- I'm so sorry.
getSurroundingMotion
    :: Window
    -> Buffer
    -> Text
    -> Position' LineOffset a
    -> Neovim CornelisEnv ((Int64, Int64), (Int64, Int64))
getSurroundingMotion w b motion p = do
  savingCurrentWindow $ do
    savingCurrentPosition w $ do
      lc <- positionToVim <$> vimifyPositionM b p
      nvim_set_current_win w
      window_set_cursor w lc
      vim_command $ "normal v" <> motion
      start <- nvim_buf_get_mark b "<"
      end <- nvim_buf_get_mark b ">"
      nvim_input "<esc>"
      pure (start, end)

doMakeCase :: Buffer -> MakeCase -> Neovim CornelisEnv ()
doMakeCase b (RegularCase Function clauses ip) =
  replaceInterval b (ip_interval ip & #iStart . #posCol .~ Offset 1) $ T.unlines clauses
-- TODO(sandy): It would be nice if Agda just gave us the bounds we're supposed to replace...
doMakeCase b (RegularCase ExtendedLambda clauses ip) = do
  ws <- windowsForBuffer b
  case listToMaybe ws of
    Nothing ->
      vim_report_error
        "Unable to extend a lambda without having a window that contains the modified buffer. This is a bug in cornelis."
    Just w -> do
      (slsc@(sl, sc), (el, ec)) <- getSurroundingMotion w b "i}" $ iStart $ ip_interval ip
      Offset sc' <- unvimifyColumn b slsc
      nvim_buf_set_text b (sl - 1) (sc + 1) (el - 1) ec $ V.fromList $
        clauses & _tail %~ fmap (indent $ fromIntegral sc')

------------------------------------------------------------------------------
-- | Indent a string with the given offset.
indent :: Int -> Text -> Text
indent n s = T.replicate (n - 1) " " <> "; " <> s

vimifyPositionM :: Buffer -> Position' LineOffset a -> Neovim env (Position' Int64 a)
vimifyPositionM b p = do
  l <- getBufferLine b $ posLine p
  pure $ vimifyPosition l p


vimifyPosition :: Text -> Position' LineOffset a -> Position' Int64 a
vimifyPosition t = #posCol %~ fromIntegral . toBytes t

positionToVim :: Position' Int64 a -> (Int64, Int64)
positionToVim p =
  ( fromIntegral $ getLineNumber (posLine p) - 1
  , fromIntegral $ posCol p - 1
  )

getBufferLine :: Buffer -> LineNumber -> Neovim env Text
getBufferLine b (LineNumber ln) =
  buffer_get_line b $ fromIntegral ln - 1


replaceInterval :: Buffer -> Interval' LineOffset () -> Text -> Neovim CornelisEnv ()
replaceInterval b i str
  = do
    (sl, sc) <- fmap positionToVim $ vimifyPositionM b $ iStart i
    (el, ec) <- fmap positionToVim $ vimifyPositionM b $ iEnd i
    nvim_buf_set_text b sl sc el ec $ V.fromList $ T.lines str


cornelis :: Neovim () NeovimPlugin
cornelis = do
  (inchan, outchan) <- liftIO newChan
  ns <- nvim_create_namespace "cornelis"
  mvar <- liftIO $ newMVar $ CornelisState mempty

  let env = CornelisEnv mvar inchan ns
  withLocalEnv env $
    neovimAsync $ do
      forever $ reportExceptions $ do
        AgdaResp buffer next <- liftIO $ readChan outchan
        respond buffer next

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

