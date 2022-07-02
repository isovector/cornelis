{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Goals where

import           Control.Arrow ((&&&))
import           Control.Lens
import           Cornelis.Agda (withAgda)
import           Cornelis.Offsets
import           Cornelis.Types
import           Cornelis.Utils
import           Cornelis.Vim
import           Data.Foldable (toList, fold)
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Text as T
import           Data.Traversable (for)
import           Neovim
import           Neovim.API.Text
import           Neovim.User.Input (input)


------------------------------------------------------------------------------
-- | Get the spanning interval of an interaction point. If we already have
-- highlighting information from vim, use the extmark for this goal, otherwise
-- using the interval that Agda knows about.
getIpInterval :: Buffer -> InteractionPoint Identity -> Neovim CornelisEnv AgdaInterval
getIpInterval b ip = do
  ns <- asks ce_namespace
  extmap <- withBufferStuff b $ pure . bs_ip_exts
  fmap (fromMaybe (ip_interval' ip)) $
    case flip M.lookup extmap $ ip_id ip of
      Just i -> getExtmarkIntervalById ns b i
      Nothing -> pure Nothing


--------------------------------------------------------------------------------
-- | Move the vim cursor to a goal in the current window
findGoal :: Ord a => (AgdaPos -> AgdaPos -> Maybe a) -> Neovim CornelisEnv ()
findGoal hunt = withAgda $ do
  w <- vim_get_current_window
  b <- window_get_buffer w
  withBufferStuff b $ \bs -> do
    pos <- getWindowCursor w
    let goals = toList $ bs_ips bs
    judged_goals <- fmap catMaybes $ for goals $ \ip -> do
      int <- getIpInterval b ip
      pure
        . sequenceA
        . (id &&& hunt pos)
        $ iStart int
    case judged_goals of
      [] -> reportInfo "No hole matching predicate"
      _ -> do
        let pos' = fst $ maximumBy (comparing snd) judged_goals
        setWindowCursor w pos'


------------------------------------------------------------------------------
-- | Move the vim cursor to the previous interaction point.
prevGoal :: Neovim CornelisEnv ()
prevGoal =
  findGoal $ \pos goal ->
    case pos > goal of
      False -> Nothing
      True -> Just $ ( p_line goal .-. p_line pos
                     , p_col goal .-. p_col pos  -- TODO: This formula looks fishy
                     )


------------------------------------------------------------------------------
-- | Move the vim cursor to the next interaction point.
nextGoal :: Neovim CornelisEnv ()
nextGoal =
  findGoal $ \pos goal ->
    case pos < goal of
      False -> Nothing
      True -> Just $ Down ( p_line goal .-. p_line pos
                          , p_col goal .-. p_col pos
                          )

------------------------------------------------------------------------------
-- | Uses highlighting extmarks to determine what a hole is; since the user
-- might have typed inside of a {! !} goal since they last saved.
getGoalAtCursor :: Neovim CornelisEnv (Buffer, Maybe (InteractionPoint Identity))
getGoalAtCursor = do
  w <- nvim_get_current_win
  b <- window_get_buffer w
  p <- getWindowCursor w
  fmap (b, ) $ getGoalAtPos b p


getGoalAtPos
    :: Buffer
    -> AgdaPos
    -> Neovim CornelisEnv (Maybe (InteractionPoint Identity))
getGoalAtPos b p = do
  fmap (getFirst . fold) $ withBufferStuff b $ \bs -> do
    for (bs_ips bs) $ \ip -> do
      int <- getIpInterval b ip
      pure $ case containsPoint int p of
        False -> mempty
        True -> pure $ ip { ip_intervalM = Identity int }

------------------------------------------------------------------------------
-- | Run a continuation on a goal at the current position in the current
-- buffer, if it exists.
withGoalAtCursor
    :: (Buffer -> InteractionPoint Identity -> Neovim CornelisEnv a)
    -> Neovim CornelisEnv (Maybe a)
withGoalAtCursor f = getGoalAtCursor >>= \case
   (_, Nothing) -> do
     reportInfo "No goal at cursor"
     pure Nothing
   (b, Just ip) -> fmap Just $ f b ip

------------------------------------------------------------------------------
-- | Run the first continuation on the goal at the current position,
-- otherwise run the second continuation.
--
-- If there is a non-empty hole, provide its content to the first continuation.
-- If the hole is empty, prompt for input and provide that.
--
-- If there is no goal, prompt the user for input and run the second continuation.
withGoalContentsOrPrompt
    :: String
    -- ^ Text to print when prompting the user for input
    -> (InteractionPoint Identity -> String -> Neovim CornelisEnv a)
    -- ^ Continuation to run on goal, with hole contents
    -> (String -> Neovim CornelisEnv a)
    -- ^ Continuation to run on user input if there's no goal here
    -> Neovim CornelisEnv a
withGoalContentsOrPrompt prompt_str on_goal on_no_goal = getGoalAtCursor >>= \case
    (_, Nothing) ->
        -- If there's no goal here, run `on_no_goal` on user input.
        prompt >>= on_no_goal
    (b, Just ip) -> do
        content <- getGoalContents b ip
        -- If there is a goal under the cursor, but it's contents
        -- are empty, prompt the user for input.  Otherwise, unpack
        -- the hole contents are provide that.
        if T.null content
            then prompt >>= on_goal ip
            else on_goal ip (T.unpack content)
    where
        prompt = input prompt_str Nothing Nothing


------------------------------------------------------------------------------
-- | Get the contents of a goal.
getGoalContents_maybe :: Buffer -> InteractionPoint Identity -> Neovim CornelisEnv (Maybe Text)
getGoalContents_maybe b ip = do
  int <- getIpInterval b ip
  iv <- fmap T.strip $ getBufferInterval b int
  pure $ case iv of
    "?" -> Nothing
         -- Chop off {!, !} and trim any spaces.
    _ -> Just $ T.strip $ T.dropEnd 2 $ T.drop 2 $ iv


------------------------------------------------------------------------------
-- | Like 'getGoalContents_maybe'.
getGoalContents :: Buffer -> InteractionPoint Identity -> Neovim CornelisEnv Text
getGoalContents b ip = fromMaybe "" <$> getGoalContents_maybe b ip


------------------------------------------------------------------------------
-- | Replace all single @?@ tokens with interaction holes.
replaceQuestion :: Text -> Text
replaceQuestion = T.unwords . fmap go . T.words
  where
    go "(?" = "({! !}"
    go "?" = "{! !}"
    go x   =
      case T.dropWhileEnd (== ')') x of
        "?" -> "{! !}" <> T.drop 1 x
        _ -> x

