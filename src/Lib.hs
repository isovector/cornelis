{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Lib where

import           Control.Arrow ((&&&))
import           Control.Concurrent (newMVar)
import           Control.Concurrent.Chan.Unagi
import           Control.Lens
import           Control.Monad (forever)
import           Control.Monad (when)
import           Control.Monad.State.Class (gets)
import           Cornelis.Config (getConfig)
import           Cornelis.Debug (reportExceptions)
import           Cornelis.Goals
import           Cornelis.Highlighting (highlightBuffer, getLineIntervals, lookupPoint)
import           Cornelis.InfoWin
import           Cornelis.Offsets
import           Cornelis.Types
import           Cornelis.Utils
import           Cornelis.Vim
import           Data.Bifunctor
import           Data.Foldable (for_)
import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import qualified Data.Text as T
import           Neovim
import           Neovim.API.Text
import           Neovim.Plugin (CommandOption(CmdComplete))
import           Plugin



getInteractionPoint :: Buffer -> Int -> Neovim CornelisEnv (Maybe (InteractionPoint Identity LineOffset))
getInteractionPoint b i = gets $ preview $ #cs_buffers . ix b . #bs_ips . ix i


respondToHelperFunction :: DisplayInfo -> Neovim env ()
respondToHelperFunction (HelperFunction sig) = setreg "\"" sig
respondToHelperFunction _ = pure ()


respond :: Buffer -> Response -> Neovim CornelisEnv ()
-- Update the buffer's goal map
respond b (DisplayInfo dp) = do
  respondToHelperFunction dp
  when (dp & hasn't #_GoalSpecific) $
    modifyBufferStuff b $ #bs_goals .~ dp
  goalWindow b dp
-- Update the buffer's interaction points map
respond b (InteractionPoints ips) = do
  let ips' = mapMaybe sequenceInteractionPoint ips
  modifyBufferStuff b $ #bs_ips .~ (IM.fromList $ fmap (ip_id &&& id) $ fmap (fmap agdaToLine) ips')
-- Replace a function clause
respond b (MakeCase mkcase) = do
  doMakeCase b mkcase
  reload
-- Replace the interaction point with a result
respond b (GiveAction result ip) = do
  let i = ip_id ip
  getInteractionPoint b i >>= \case
    Nothing -> reportError $ T.pack $ "Can't find interaction point " <> show i
    Just ip' ->
      replaceInterval b (iStart $ ip_interval ip') (iEnd $ ip_interval ip')
        $ replaceQuestion result
  reload
-- Replace the interaction point with a result
respond b (SolveAll solutions) = do
  for_ solutions $ \(Solution i ex) ->
    getInteractionPoint b i >>= \case
      Nothing -> reportError $ T.pack $ "Can't find interaction point " <> show i
      Just ip -> do
        replaceInterval b (iStart $ ip_interval ip) (iEnd $ ip_interval ip)
          $ replaceQuestion ex
  reload
respond b ClearHighlighting = do
  -- delete what we know about goto positions
  modifyBufferStuff b $ #bs_goto_sites .~ mempty
  -- remove the extmarks and highlighting
  ns <- asks ce_namespace
  nvim_buf_clear_namespace b ns 0 (-1)
respond b (HighlightingInfo _remove hl) =
  void $ highlightBuffer b hl
respond _ (RunningInfo _ x) = reportInfo x
respond _ (ClearRunningInfo) = reportInfo ""
respond b (JumpToError _ pos) = do
  buf_lines <- nvim_buf_get_lines b 0 (-1) True
  let li = getLineIntervals buf_lines
  case lookupPoint li pos of
    Nothing -> reportError "invalid error report from Agda"
    Just lc -> do
      ws <- fmap listToMaybe $ windowsForBuffer b
      for_ ws
        $ flip window_set_cursor
        $ first (fromIntegral . getOneIndexedLineNumber . incLineNumber) lc
respond _ Status{} = pure ()
respond _ (Unknown k _) = reportError k

doMakeCase :: Buffer -> MakeCase -> Neovim CornelisEnv ()
doMakeCase b (RegularCase Function clauses ip) = do
  int' <- traverseInterval (pure . fmap agdaToLine) $ ip_interval ip
  let int :: Interval' LineOffset
      int = int' & #iStart . #p_col .~ Offset 0
      start = iStart int
      end = iEnd int
  ins <- getIndent b $ p_line start
  replaceInterval b start end
    $ T.unlines
    $ fmap (T.replicate ins " " <>)
    $ fmap replaceQuestion clauses
-- TODO(sandy): It would be nice if Agda just gave us the bounds we're supposed to replace...
doMakeCase b (RegularCase ExtendedLambda clauses ip) = do
  let ip' = fmap agdaToLine ip
  ws <- windowsForBuffer b
  case listToMaybe ws of
    Nothing ->
      reportError
        "Unable to extend a lambda without having a window that contains the modified buffer. This is a limitation in cornelis."
    Just w -> do
      (start, end)
        <- getSurroundingMotion w b "i}"
          -- Subtract one so we are outside of a {! !} goal and the i} movement
          -- works correctly
         $ fmap (offsetSubtract 1)
         $ iStart
         $ ip_interval ip'
      -- Add an extra character to the start so we leave a space after the
      -- opening brace
      replaceInterval b (start & #p_col %~ offsetPlus (Offset 1)) end
        $ T.unlines
        $ fmap replaceQuestion clauses & _tail %~ fmap (indent start)


------------------------------------------------------------------------------
-- | Indent a string with the given offset.
indent :: Pos -> Text -> Text
indent (Pos _ (Offset n)) s = T.replicate (fromIntegral n - 1) " " <> "; " <> s


doPrevGoal :: CommandArguments -> Neovim CornelisEnv ()
doPrevGoal = const prevGoal

doNextGoal :: CommandArguments -> Neovim CornelisEnv ()
doNextGoal = const nextGoal


cornelisInit :: Neovim env CornelisEnv
cornelisInit = do
  (inchan, outchan) <- liftIO newChan
  ns <- nvim_create_namespace "cornelis"
  mvar <- liftIO $ newMVar $ CornelisState mempty

  cfg <- getConfig

  let env = CornelisEnv mvar inchan ns cfg
  void $ withLocalEnv env $
    neovimAsync $ do
      forever $ reportExceptions $ do
        AgdaResp buffer next <- liftIO $ readChan outchan
        void $ neovimAsync $ reportExceptions $ respond buffer next
  pure env


-- Flush the TH environment
$(pure [])


main :: IO ()
main = neovim defaultConfig { plugins = [cornelis] }


cornelis :: Neovim () NeovimPlugin
cornelis = do
  env <- cornelisInit
  closeInfoWindows

  let rw_complete = CmdComplete "custom,InternalCornelisRewriteModeCompletion"
      cm_complete = CmdComplete "custom,InternalCornelisComputeModeCompletion"

  wrapPlugin $ Plugin
    { environment = env
    , exports =
        [ $(command "CornelisRestart"          'doRestart)        [CmdSync Async]
        , $(command "CornelisAbort"            'doAbort)          [CmdSync Async]
        , $(command "CornelisLoad"             'doLoad)           [CmdSync Async]
        , $(command "CornelisGoals"            'doAllGoals)       [CmdSync Async]
        , $(command "CornelisSolve"            'solveOne)         [CmdSync Async, rw_complete]
        , $(command "CornelisAuto"             'autoOne)          [CmdSync Async]
        , $(command "CornelisTypeContext"      'typeContext)      [CmdSync Async, rw_complete]
        , $(command "CornelisTypeContextInfer" 'typeContextInfer) [CmdSync Async, rw_complete]
        , $(command "CornelisMakeCase"         'doCaseSplit)      [CmdSync Async]
        , $(command "CornelisRefine"           'doRefine)         [CmdSync Async]
        , $(command "CornelisGive"             'doGive)           [CmdSync Async]
        , $(command "CornelisElaborate"        'doElaborate)      [CmdSync Async, rw_complete]
        , $(command "CornelisPrevGoal"         'doPrevGoal)       [CmdSync Async]
        , $(command "CornelisNextGoal"         'doNextGoal)       [CmdSync Async]
        , $(command "CornelisGoToDefinition"   'doGotoDefinition) [CmdSync Async]
        , $(command "CornelisWhyInScope"       'doWhyInScope)     [CmdSync Async]
        , $(command "CornelisNormalize"        'doNormalize)      [CmdSync Async, cm_complete]
        , $(command "CornelisHelperFunc"       'doHelperFunc)     [CmdSync Async, rw_complete]
        , $(command "CornelisQuestionToMeta"   'doQuestionToMeta) [CmdSync Async]
        , $(function "InternalCornelisRewriteModeCompletion" 'rewriteModeCompletion) Sync
        , $(function "InternalCornelisComputeModeCompletion" 'computeModeCompletion) Sync
        ]
    }

