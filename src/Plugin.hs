{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Plugin where

import           Control.Lens
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Cornelis.Agda (spawnAgda, withCurrentBuffer, runIOTCM)
import           Cornelis.Highlighting (getExtmarks)
import           Cornelis.InfoWin (buildInfoBuffer, showInfoWindow)
import           Cornelis.Offsets
import           Cornelis.Pretty (prettyGoals)
import           Cornelis.Types
import           Cornelis.Types.Agda hiding (Error)
import           Cornelis.Utils
import           Cornelis.Vim
import           Data.Foldable (for_, toList, fold)
import           Data.List
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Ord
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text
import           Neovim.User.Input (input)
import           System.Process (terminateProcess)
import           Text.Read (readMaybe)



------------------------------------------------------------------------------
-- | Ensure we have a 'BufferStuff' attached to the current buffer.
withAgda :: Neovim CornelisEnv a -> Neovim CornelisEnv a
withAgda m = do
  buffer <- vim_get_current_buffer
  gets (M.lookup buffer . cs_buffers) >>= \case
    Just _ -> m
    Nothing -> do
      agda <- spawnAgda buffer
      iw <- buildInfoBuffer
      modify' $ #cs_buffers %~ M.insert buffer BufferStuff
        { bs_agda_proc = agda
        , bs_ips = mempty
        , bs_goto_sites = mempty
        , bs_goals = AllGoalsWarnings [] [] [] []
        , bs_info_win = iw
        }
      m

getAgda :: Buffer -> Neovim CornelisEnv Agda
getAgda buffer = gets $ bs_agda_proc . (M.! buffer) . cs_buffers


getGoalAtCursor :: Neovim CornelisEnv (Buffer, Maybe (InteractionPoint Identity LineOffset))
getGoalAtCursor = do
  w <- nvim_get_current_win
  b <- window_get_buffer w
  p <- getWindowCursor w
  ips <- fmap bs_ips . M.lookup b <$> gets cs_buffers
  pure (b, ips >>= flip lookupGoal p)


lookupGoal :: Foldable t => t (InteractionPoint Identity LineOffset) -> Pos -> Maybe (InteractionPoint Identity LineOffset)
lookupGoal ips p = flip find ips $ (\(InteractionPoint _ (Identity iv)) -> containsPoint iv p)


withGoalAtCursor :: (Buffer -> InteractionPoint Identity LineOffset -> Neovim CornelisEnv a) -> Neovim CornelisEnv (Maybe a)
withGoalAtCursor f = getGoalAtCursor >>= \case
   (_, Nothing) -> do
     reportInfo "No goal at cursor"
     pure Nothing
   (b, Just ip) -> fmap Just $ f b ip


getGoalContents :: Buffer -> InteractionPoint Identity LineOffset -> Neovim CornelisEnv Text
getGoalContents b ip = do
  iv <- getBufferInterval b $ ip_interval ip
  -- Chop off {!, !} and trim any spaces.
  -- Unclear why this is dropEnd 3 instead of 2, but it works.
  pure $ T.strip $ T.dropEnd 3 $ T.drop 2 $ iv


getDefinitionSites :: Buffer -> Pos -> Neovim CornelisEnv (First DefinitionSite)
getDefinitionSites b p = withBufferStuff b $ \bs -> do
  marks <- getExtmarks b p
  pure $ flip foldMap marks $ \es ->
    First $ M.lookup (es_mark es) $ bs_goto_sites bs


doGotoDefinition :: CommandArguments -> Neovim CornelisEnv ()
doGotoDefinition _ = gotoDefinition

gotoDefinition :: Neovim CornelisEnv ()
gotoDefinition = withAgda $ do
  w <- nvim_get_current_win
  rc <- getWindowCursor w
  b <- window_get_buffer w
  getDefinitionSites b rc >>= \case
    First Nothing -> reportInfo "No syntax under cursor."
    First (Just ds) -> do
      -- TODO(sandy): escape spaces
      vim_command $ "edit " <> ds_filepath ds
      b' <- window_get_buffer w
      contents <- fmap (T.unlines . V.toList) $ buffer_get_lines b' 0 (-1) False
      let buffer_idx = toBytes contents $ ds_position ds
      -- TODO(sandy): use window_set_cursor instead?
      vim_command $ "keepjumps normal! " <> T.pack (show buffer_idx) <> "go"

reload :: Neovim CornelisEnv ()
reload = do
  vim_command "noautocmd w"
  load


doLoad :: CommandArguments -> Neovim CornelisEnv ()
doLoad = const load

load :: Neovim CornelisEnv ()
load = withAgda $ do
  agda <- withCurrentBuffer getAgda
  name <- buffer_get_name $ a_buffer agda
  flip runIOTCM agda $ Cmd_load name []

doAllGoals :: CommandArguments -> Neovim CornelisEnv ()
doAllGoals = const allGoals

allGoals :: Neovim CornelisEnv ()
allGoals =
  withAgda $ withCurrentBuffer $ \b ->
    withBufferStuff b $ \bs -> do
      goalWindow b $ bs_goals bs

doRestart :: CommandArguments -> Neovim CornelisEnv ()
doRestart _ = do
  bs <- gets cs_buffers
  modify $ #cs_buffers .~ mempty
  liftIO $ for_ bs $ terminateProcess . a_hdl . bs_agda_proc


normalizationMode :: Neovim env Rewrite
normalizationMode = pure Normalised


solveOne :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
solveOne _ ms = withNormalizationMode ms $ \mode ->
  withAgda $ void $ withGoalAtCursor $ \b goal -> do
    agda <- getAgda b
    flip runIOTCM agda $ Cmd_solveOne mode (InteractionId $ ip_id goal) noRange ""

autoOne :: CommandArguments -> Neovim CornelisEnv ()
autoOne _ = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  t <- getGoalContents b goal
  flip runIOTCM agda $ Cmd_autoOne (InteractionId $ ip_id goal) noRange $ T.unpack t

withNormalizationMode :: Maybe String -> (Rewrite -> Neovim e ()) -> Neovim e ()
withNormalizationMode Nothing f = normalizationMode >>= f
withNormalizationMode (Just s) f =
  case readMaybe s of
    Nothing -> reportError $ "Invalid normalization mode: " <> T.pack s
    Just nm -> f nm

typeContext :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
typeContext _ ms = withNormalizationMode ms $ \mode ->
  withAgda $ void $ withGoalAtCursor $ \b goal -> do
    agda <- getAgda b
    flip runIOTCM agda $ Cmd_goal_type_context mode (InteractionId $ ip_id goal) noRange ""

doRefine :: CommandArguments -> Neovim CornelisEnv ()
doRefine = const refine

refine :: Neovim CornelisEnv ()
refine = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  t <- getGoalContents b goal
  flip runIOTCM agda $ Cmd_refine_or_intro True (InteractionId $ ip_id goal) noRange $ T.unpack t

doWhyInScope :: CommandArguments -> Neovim CornelisEnv ()
doWhyInScope _ = do
  thing <- input "Why is what in scope? " Nothing Nothing
  whyInScope thing

whyInScope :: Text -> Neovim CornelisEnv ()
whyInScope thing = do
  withAgda $ void $ withCurrentBuffer $ \b -> do
    agda <- getAgda b
    flip runIOTCM agda $ Cmd_why_in_scope_toplevel $ T.unpack thing

doNormalize :: CommandArguments -> Neovim CornelisEnv ()
doNormalize _ = do
  withAgda $ void $ withCurrentBuffer $ \b -> do
    thing <- input "Normalize what? " Nothing Nothing
    agda <- getAgda b
    flip runIOTCM agda $ Cmd_compute_toplevel DefaultCompute thing

helperFunc :: Rewrite -> Text -> Neovim CornelisEnv ()
helperFunc mode expr = do
  withAgda $ void $ withGoalAtCursor $ \b goal -> do
    agda <- getAgda b
    flip runIOTCM agda $ Cmd_helper_function mode (InteractionId $ ip_id goal) noRange $ T.unpack expr

doHelperFunc :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
doHelperFunc _ ms = withNormalizationMode ms $ \mode -> do
  expr <- input "Expression: " Nothing Nothing
  helperFunc mode expr

doCaseSplit :: CommandArguments -> Neovim CornelisEnv ()
doCaseSplit _ = do
  thing <- input @Text "Split on what?" Nothing Nothing
  caseSplit thing

caseSplit :: Text -> Neovim CornelisEnv ()
caseSplit thing = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  flip runIOTCM agda $ Cmd_make_case (InteractionId $ ip_id goal) noRange $ T.unpack thing

goalWindow :: Buffer -> DisplayInfo ->  Neovim CornelisEnv ()
goalWindow b = showInfoWindow b . prettyGoals

