{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Plugin where

import           Control.Lens
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Cornelis.Agda (spawnAgda, withCurrentBuffer, runIOTCM)
import           Cornelis.InfoWin (buildInfoBuffer, showInfoWindow)
import           Cornelis.Offsets
import           Cornelis.Pretty (prettyGoals)
import           Cornelis.Types
import           Cornelis.Types.Agda hiding (Error)
import           Cornelis.Utils
import           Cornelis.Vim
import           Data.List
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text
import           Neovim.User.Input (input)



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


getGoalAtCursor :: Neovim CornelisEnv (Buffer, Maybe (InteractionPoint LineOffset))
getGoalAtCursor = do
  w <- nvim_get_current_win
  b <- window_get_buffer w
  p <- getWindowCursor w
  ips <- fmap bs_ips . M.lookup b <$> gets cs_buffers
  pure (b, ips >>= flip lookupGoal p)


lookupGoal :: Foldable t => t (InteractionPoint LineOffset) -> Pos -> Maybe (InteractionPoint LineOffset)
lookupGoal ips p = flip find ips $ (\(InteractionPoint _ iv) -> containsPoint iv p)

containsPoint :: Ord a => Interval' a -> Pos' a -> Bool
containsPoint (Interval s e) (posToPosition -> p) = s <= p && p < e


withGoalAtCursor :: (Buffer -> InteractionPoint LineOffset -> Neovim CornelisEnv a) -> Neovim CornelisEnv (Maybe a)
withGoalAtCursor f = getGoalAtCursor >>= \case
   (_, Nothing) -> do
     reportInfo "No goal at cursor"
     pure Nothing
   (b, Just ip) -> fmap Just $ f b ip


parseExtmark :: Buffer -> Object -> Neovim CornelisEnv (Maybe (Extmark, Interval' LineOffset))
parseExtmark b
  (ObjectArray ( (objectToInt -> Just ext)
               : (objectToInt -> Just line)
               : (objectToInt -> Just col)
               : ObjectMap details
               : _
               )) = runMaybeT $ do
  vim_end_col <- hoistMaybe $ objectToInt =<< M.lookup (ObjectString "end_col") details
  -- Plus one here because our lines are 1-indexed but the results of
  -- get_extmarks is 0-indexed.
  let start_line = LineNumber $ fromIntegral $ line + 1
  end_line <- hoistMaybe $ fmap (LineNumber . (+1) . fromIntegral)
            . objectToInt =<< M.lookup (ObjectString "end_row") details
  sc <- lift $ unvimifyColumn b start_line $ fromIntegral col
  ec <- lift $ unvimifyColumn b end_line   $ fromIntegral vim_end_col
  pure ( Extmark $ fromIntegral ext
       , Interval { iStart = posToPosition $ Pos start_line sc
                  , iEnd   = posToPosition $ Pos end_line ec
                  }
       )
parseExtmark _ _ = pure Nothing


hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure


getExtmark :: Buffer -> Pos -> Neovim CornelisEnv (First DefinitionSite)
getExtmark b p = withBufferStuff b $ \bs -> do
  ns <- asks ce_namespace
  vp <- vimifyPositionM b p
  -- The vim API uses 0-indexed lines for buf_get_extmarks..
  let pos0 = ObjectArray [ ObjectInt $ getVimLineNumber $ p_line vp
                         , ObjectInt 0 -- from the beginning of the line
                         ]
      pos1 = ObjectArray [ ObjectInt $ getVimLineNumber $ p_line vp
                         , ObjectInt (-1) -- to the end of the line
                         ]
  res <- nvim_buf_get_extmarks b ns pos0 pos1 $ M.singleton "details" $ ObjectBool True
  marks <- fmap catMaybes $ traverse (parseExtmark b) $ V.toList res

  pure $ flip foldMap marks $ \(ex, i) ->
    case containsPoint i p of
      False -> mempty
      True -> First $ M.lookup ex $ bs_goto_sites bs

gotoDefinition :: CommandArguments -> Neovim CornelisEnv ()
gotoDefinition _ = withAgda $ do
  w <- nvim_get_current_win
  rc <- getWindowCursor w
  b <- window_get_buffer w
  getExtmark b rc >>= \case
    First Nothing -> reportInfo "No syntax under cursor."
    First (Just ds) -> do
      -- TODO(sandy): escape spaces
      vim_command $ "edit " <> ds_filepath ds
      b' <- window_get_buffer w
      contents <- fmap (T.unlines . V.toList) $ buffer_get_lines b' 0 (-1) False
      let buffer_idx = toBytes contents $ ds_position ds
      -- TODO(sandy): use window_set_cursor instead?
      vim_command $ "normal! " <> T.pack (show buffer_idx) <> "go"


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


solveOne :: CommandArguments -> Neovim CornelisEnv ()
solveOne _ = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  flip runIOTCM agda $ Cmd_solveOne Simplified (InteractionId $ ip_id goal) noRange ""

typeContext :: CommandArguments -> Neovim CornelisEnv ()
typeContext _ = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  flip runIOTCM agda $ Cmd_goal_type_context Simplified (InteractionId $ ip_id goal) noRange ""

doRefine :: CommandArguments -> Neovim CornelisEnv ()
doRefine = const refine

refine :: Neovim CornelisEnv ()
refine = withAgda $ void $ withGoalAtCursor $ \b goal -> do
  agda <- getAgda b
  flip runIOTCM agda $ Cmd_refine_or_intro True (InteractionId $ ip_id goal) noRange ""

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

