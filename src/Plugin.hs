{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Plugin where

import           Control.Lens
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Cornelis.Agda (withCurrentBuffer, runIOTCM, withAgda, getAgda)
import           Cornelis.Goals
import           Cornelis.Highlighting (getExtmarks, highlightInterval)
import           Cornelis.InfoWin (showInfoWindow)
import           Cornelis.Offsets
import           Cornelis.Pretty (prettyGoals, HighlightGroup (Todo))
import           Cornelis.Types
import           Cornelis.Types.Agda hiding (Error)
import           Cornelis.Utils
import           Cornelis.Vim
import           Data.Foldable (for_, fold, toList)
import qualified Data.IntMap as IM
import           Data.List
import qualified Data.Map as M
import           Data.Ord
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text
import           Neovim.User.Input (input)
import           System.Process (terminateProcess)
import           Text.Read (readMaybe)



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
load = withAgda $ withCurrentBuffer $ \b -> do
  agda <- getAgda b
  name <- buffer_get_name $ a_buffer agda
  flip runIOTCM agda $ Cmd_load name []

questionToMeta :: Buffer -> Neovim CornelisEnv ()
questionToMeta b = withBufferStuff b $ \bs -> do
  let ips = toList $ bs_ips bs

  res <- fmap fold $ for (sortOn (Down . iStart . ip_interval) ips) $ \ip -> do
    let int = ip_interval ip
    getGoalContents_maybe b int >>= \case
      -- We only don't have a goal contents if we are a ? goal
      Nothing -> do
        replaceInterval b (iStart int) (iEnd int) "{! !}"
        let int' = int
                  { iEnd = (iStart int)
                              -- Inclusive, so we add only 4 offset, rather
                              -- than the 5 for the characters
                    { p_col = offsetPlus (p_col $ iStart int) (Offset 4)
                    }
                  }
        void $ highlightInterval b int' Todo
        modifyBufferStuff b $
          #bs_ips %~ IM.insert (ip_id ip) (ip & #ip_interval' . #_Identity .~ int')

        pure $ Any True
      Just _ -> pure $ Any False

  -- Force a save if we replaced any goals
  case getAny res of
    True -> reload
    False -> pure ()


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
normalizationMode = pure HeadNormal


solveOne :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
solveOne _ ms = withNormalizationMode ms $ \mode ->
  withAgda $ void $ withGoalAtCursor $ \b goal -> do
    agda <- getAgda b
    flip runIOTCM agda $ Cmd_solveOne mode (InteractionId $ ip_id goal) noRange ""

autoOne :: CommandArguments -> Neovim CornelisEnv ()
autoOne _ = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  t <- getGoalContents b $ ip_interval goal
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

typeContextInfer :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
typeContextInfer _ ms = withNormalizationMode ms $ \mode ->
  withAgda $ void $ withGoalAtCursor $ \b goal -> do
    agda <- getAgda b
    contents <- getGoalContents b $ ip_interval goal
    flip runIOTCM agda
      $ Cmd_goal_type_context_infer mode (InteractionId $ ip_id goal) noRange
      $ T.unpack contents

doRefine :: CommandArguments -> Neovim CornelisEnv ()
doRefine = const refine

refine :: Neovim CornelisEnv ()
refine = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  t <- getGoalContents b $ ip_interval goal
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

doQuestionToMeta :: CommandArguments -> Neovim CornelisEnv ()
doQuestionToMeta _ = withCurrentBuffer questionToMeta

goalWindow :: Buffer -> DisplayInfo ->  Neovim CornelisEnv ()
goalWindow b = showInfoWindow b . prettyGoals

