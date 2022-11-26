{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Goals where

import           Control.Arrow ((&&&))
import           Control.Lens
import           Cornelis.Agda (withAgda)
import           Cornelis.Debug (debug)
import           Cornelis.Highlighting (getExtmarks, holeHlGroup)
import           Cornelis.Offsets
import           Cornelis.Types
import           Cornelis.Utils
import           Cornelis.Vim
import           Data.Foldable (toList, fold)
import qualified Data.IntMap as IM
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Text as T
import           Data.Traversable (for)
import           Neovim
import           Neovim.API.Text


--------------------------------------------------------------------------------
-- | Move the vim cursor to a goal in the current window
findGoal :: Ord a => (AgdaPos -> AgdaPos -> Maybe a) -> Neovim CornelisEnv ()
findGoal hunt = withAgda $ do
  w <- vim_get_current_window
  b <- window_get_buffer w
  withBufferStuff b $ \bs -> do
    pos <- getWindowCursor w
    let goals = toList $ bs_ips bs
        judged_goals
              = mapMaybe ( sequenceA
                         . (id &&& hunt pos)
                         . iStart
                         . ip_interval
                         ) goals
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


------------------------------------------------------------------------------
-- | VERY BIG HACK
--
-- Not only does this get the goal at the position, it also updates the
-- internal state tracking where the goal is!!!
getGoalAtPos
    :: Buffer
    -> AgdaPos
    -> Neovim CornelisEnv (Maybe (InteractionPoint Identity))
getGoalAtPos b p = do
  debug ("getgoalat", p)
  z <- withBufferStuff b $ \bs -> do
    marks <- getExtmarks b p
    debug ("marks", marks)
    let todo = T.pack $ show holeHlGroup

    fmap fold $ for marks $ \es -> do
      case es_hlgroup es == todo of
        False -> pure mempty
        True -> do
          case find ((== iStart (es_interval es)) . iStart . ip_interval)
                  $ toList (bs_ips bs) of
            Nothing -> pure mempty
            Just ip -> do
              let ip' = ip { ip_interval' = Identity (es_interval es) }
              -- BIG HACK!!
              -- This is a convenient place to update our global mapping of
              -- where our holes are, since we just found one.
              modifyBufferStuff b $ #bs_ips %~ IM.insert (ip_id ip') ip'
              pure $ pure ip'

  pure $ getFirst z


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
-- | Get the contents of a goal. PRECONDITION: The given interval correctly
-- spans an interaction point.
--
-- TODO(sandy): make this call correct by construction
getGoalContents_maybe :: Buffer -> AgdaInterval -> Neovim CornelisEnv (Maybe Text)
getGoalContents_maybe b ip = do
  iv <- fmap T.strip $ getBufferInterval b ip
  pure $ case iv of
    "?" -> Nothing
         -- Chop off {!, !} and trim any spaces.
    _ -> Just $ T.strip $ T.dropEnd 2 $ T.drop 2 $ iv


------------------------------------------------------------------------------
-- | Like 'getGoalContents_maybe', subject to the same limitations.
getGoalContents :: Buffer -> AgdaInterval -> Neovim CornelisEnv Text
getGoalContents b ip = fromMaybe "" <$> getGoalContents_maybe b ip


------------------------------------------------------------------------------
-- | Replace all single @?@ tokens with interaction holes.
replaceQuestion :: Text -> Text
replaceQuestion = T.unwords . fmap go . T.words
  where
    go "?" = "{! !}"
    go x   =
      case T.dropWhileEnd (== ')') x of
        "?" -> "{! !}" <> T.drop 1 x
        _ -> x

