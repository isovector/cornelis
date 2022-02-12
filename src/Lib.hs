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
import           Control.Monad.Reader (withReaderT)
import           Control.Monad.State.Class (gets)
import           Control.Monad.Trans.Resource (transResourceT)
import           Cornelis.Debug (reportExceptions)
import           Cornelis.Highlighting (highlightBuffer, getLineIntervals, lookupPoint)
import           Cornelis.InfoWin
import           Cornelis.Offsets
import           Cornelis.Types
import           Cornelis.Types.Agda
import           Cornelis.Utils
import           Cornelis.Vim
import           Data.Bifunctor
import           Data.Foldable (for_, toList, maximumBy)
import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import           Data.Ord (comparing, Down (Down))
import qualified Data.Text as T
import           Neovim
import           Neovim.API.Text
import           Neovim.Context.Internal (Neovim(..), retypeConfig)
import           Plugin



withLocalEnv :: env -> Neovim env a -> Neovim env' a
withLocalEnv env (Neovim t) = Neovim . flip transResourceT t $ withReaderT (retypeConfig env)


getInteractionPoint :: Buffer -> Int -> Neovim CornelisEnv (Maybe (InteractionPoint LineOffset))
getInteractionPoint b i = gets $ preview $ #cs_buffers . ix b . #bs_ips . ix i


respond :: Buffer -> Response -> Neovim CornelisEnv ()
-- Update the buffer's goal map
respond b (DisplayInfo dp) = do
  when (dp & hasn't #_GoalSpecific) $
    modifyBufferStuff b $ #bs_goals .~ dp
  goalWindow b dp
-- Update the buffer's interaction points map
respond b (InteractionPoints ips) = do
  modifyBufferStuff b $ #bs_ips .~ (IM.fromList $ fmap (ip_id &&& id) $ fmap (fmap agdaToLine) ips)
-- Replace a function clause
respond b (MakeCase mkcase) = do
  doMakeCase b mkcase
  reload
-- Replace the interaction point with a result
respond b (GiveAction result ip) = do
  let ip' = fmap agdaToLine ip
  replaceInterval b (positionToPos $ iStart $ ip_interval ip') (positionToPos $ iEnd $ ip_interval ip') result
  reload
-- Replace the interaction point with a result
respond b (SolveAll solutions) = do
  for_ solutions $ \(Solution i ex) -> do
    getInteractionPoint b i >>= \case
      Nothing -> reportInfo $ T.pack $ "Can't find interaction point " <> show i
      Just ip -> do
        replaceInterval b (positionToPos $ iStart $ ip_interval ip) (positionToPos $ iEnd $ ip_interval ip) $ parens ex
        reload
respond b ClearHighlighting = do
  -- delete what we know about goto positions
  modifyBufferStuff b $ #bs_goto_sites .~ mempty
  -- remove the extmarks and highlighting
  ns <- asks ce_namespace
  nvim_buf_clear_namespace b ns 0 (-1)
respond b (HighlightingInfo _remove hl) =
  highlightBuffer b hl
respond _ (RunningInfo _ x) = reportInfo x
respond _ (ClearRunningInfo) = reportInfo ""
respond b (JumpToError _ pos) = do
  buf_lines <- nvim_buf_get_lines b 0 (-1) True
  let li = getLineIntervals buf_lines
  case lookupPoint li pos of
    Nothing -> reportError "invalid error report from Agda"
    Just lc -> do
      ws <- windowsForBuffer b
      for_ ws $ flip window_set_cursor $ first (+1) lc
respond _ Status{} = pure ()
respond _ (Unknown k _) = reportError k

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
    -> Pos
    -> Neovim env (Pos, Pos)
getSurroundingMotion w b motion p = do
  savingCurrentWindow $ do
    savingCurrentPosition w $ do
      nvim_set_current_win w
      setWindowCursor w p
      vim_command $ "normal v" <> motion
      start <- getpos b 'v'
      end <- getpos b '.'
      void $ nvim_input "<esc>"
      pure (start, end)

doMakeCase :: Buffer -> MakeCase -> Neovim env ()
doMakeCase b (RegularCase Function clauses ip) = do
  let int = ip_interval
          $ fmap agdaToLine
          $ ip & #ip_interval . #iStart . #posCol .~ Offset 1
      start = positionToPos $ iStart int
      end = positionToPos $ iEnd int
  ins <- getIndent b $ p_line start
  replaceInterval b start end
    $ T.unlines
    $ fmap (T.replicate ins " " <>) clauses
-- TODO(sandy): It would be nice if Agda just gave us the bounds we're supposed to replace...
doMakeCase b (RegularCase ExtendedLambda clauses ip) = do
  let ip' = fmap agdaToLine ip
  ws <- windowsForBuffer b
  case listToMaybe ws of
    Nothing ->
      reportError
        "Unable to extend a lambda without having a window that contains the modified buffer. This is a limitation in cornelis."
    Just w -> do
      (start, end) <- getSurroundingMotion w b "i}" $ positionToPos $ iStart $ ip_interval ip'
      -- Add an extra character to the start so we leave a space after the
      -- opening brace
      replaceInterval b (start & #p_col %~ offsetPlus (Offset 1)) end $ T.unlines $
        clauses & _tail %~ fmap (indent start)

mkInterval :: Pos -> Pos -> Interval' LineOffset
mkInterval start end = Interval (posToPosition start) (posToPosition end)

------------------------------------------------------------------------------
-- | Indent a string with the given offset.
indent :: Pos -> Text -> Text
indent (Pos _ (Offset n)) s = T.replicate (fromIntegral n - 1) " " <> "; " <> s


-- | Find a goal in the current window
findGoal :: Ord a => (Pos -> Pos -> Maybe a) -> Neovim CornelisEnv ()
findGoal hunt = withAgda $ do
  w <- vim_get_current_window
  b <- window_get_buffer w
  withBufferStuff b $ \bs -> do
    pos <- getWindowCursor w
    let goals = toList $ bs_ips bs
        judged_goals
              = mapMaybe ( sequenceA
                         . (id &&& hunt pos)
                         . positionToPos
                         . iStart
                         . ip_interval
                         ) goals
    case judged_goals of
      [] -> reportInfo "No hole matching predicate\n"
      _ -> do
        let pos' = fst $ maximumBy (comparing snd) judged_goals
        setWindowCursor w pos'

prevGoal :: Neovim CornelisEnv ()
prevGoal =
  findGoal $ \pos goal ->
    case pos > goal of
      False -> Nothing
      True -> Just $ ( lineDiff (p_line goal) (p_line pos)
                     , offsetDiff (p_col goal) (p_col pos)
                     )

doPrevGoal :: CommandArguments -> Neovim CornelisEnv ()
doPrevGoal = const prevGoal

nextGoal :: Neovim CornelisEnv ()
nextGoal =
  findGoal $ \pos goal ->
    case pos < goal of
      False -> Nothing
      True -> Just $ Down ( lineDiff (p_line goal) (p_line pos)
                          , offsetDiff (p_col goal) (p_col pos)
                          )

doNextGoal :: CommandArguments -> Neovim CornelisEnv ()
doNextGoal = const nextGoal


cornelisInit :: Neovim env CornelisEnv
cornelisInit = do
  (inchan, outchan) <- liftIO newChan
  ns <- nvim_create_namespace "cornelis"
  mvar <- liftIO $ newMVar $ CornelisState mempty

  let env = CornelisEnv mvar inchan ns
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

  wrapPlugin $ Plugin
    { environment = env
    , exports =
        [ $(command "CornelisLoad" 'doLoad) [CmdSync Async]
        , $(command "CornelisGoals" 'doAllGoals) [CmdSync Async]
        , $(command "CornelisSolve" 'solveOne) [CmdSync Async]
        , $(command "CornelisAuto" 'autoOne) [CmdSync Async]
        , $(command "CornelisTypeContext" 'typeContext) [CmdSync Async]
        , $(command "CornelisMakeCase" 'doCaseSplit) [CmdSync Async]
        , $(command "CornelisRefine" 'doRefine) [CmdSync Async]
        , $(command "CornelisPrevGoal" 'doPrevGoal) [CmdSync Async]
        , $(command "CornelisNextGoal" 'doNextGoal) [CmdSync Async]
        , $(command "CornelisGoToDefinition" 'gotoDefinition) [CmdSync Async]
        ]
    }

