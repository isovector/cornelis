{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Goals where

import           Control.Arrow ((&&&))
import           Control.Lens
import           Cornelis.Agda (withAgda)
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
                         . iStart
                         . ip_interval
                         ) goals
    case judged_goals of
      [] -> reportInfo "No hole matching predicate"
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

nextGoal :: Neovim CornelisEnv ()
nextGoal =
  findGoal $ \pos goal ->
    case pos < goal of
      False -> Nothing
      True -> Just $ Down ( lineDiff (p_line goal) (p_line pos)
                          , offsetDiff (p_col goal) (p_col pos)
                          )

------------------------------------------------------------------------------
-- | Uses highlighting extmarks to determine what a hole is; since the user
-- might have typed inside of a {! !} goal since they last saved.
getGoalAtCursor :: Neovim CornelisEnv (Buffer, Maybe (InteractionPoint Identity LineOffset))
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
    -> Pos
    -> Neovim CornelisEnv (Maybe (InteractionPoint Identity LineOffset))
getGoalAtPos b p = do
  z <- withBufferStuff b $ \bs -> do
    marks <- getExtmarks b p
    let todo = T.pack $ show holeHlGroup

    fmap fold $ for marks $ \es -> do
      case es_hlgroup es == todo of
        False -> pure mempty
        True -> do
          case find ((== (iStart $ es_interval es)) . iStart . ip_interval)
                  $ toList (bs_ips bs) of
            Nothing -> pure mempty
            Just ip -> do
              let ip' = ip { ip_interval' = Identity $ es_interval es }
              -- BIG HACK!!
              -- This is a convenient place to update our global mapping of
              -- where our holes are, since we just found one.
              modifyBufferStuff b $ #bs_ips %~ IM.insert (ip_id ip') ip'
              pure $ pure ip'

  pure $ getFirst z


withGoalAtCursor :: (Buffer -> InteractionPoint Identity LineOffset -> Neovim CornelisEnv a) -> Neovim CornelisEnv (Maybe a)
withGoalAtCursor f = getGoalAtCursor >>= \case
   (_, Nothing) -> do
     reportInfo "No goal at cursor"
     pure Nothing
   (b, Just ip) -> fmap Just $ f b ip


getGoalContents_maybe :: Buffer -> Interval' LineOffset -> Neovim CornelisEnv (Maybe Text)
getGoalContents_maybe b ip = do
  iv <- fmap T.strip $ getBufferInterval b ip
  pure $ case iv of
    "?" -> Nothing
         -- Chop off {!, !} and trim any spaces.
    _ -> Just $ T.strip $ T.dropEnd 2 $ T.drop 2 $ iv

getGoalContents :: Buffer -> Interval' LineOffset -> Neovim CornelisEnv Text
getGoalContents b ip = fromMaybe "" <$> getGoalContents_maybe b ip

replaceQuestion :: Text -> Text
replaceQuestion = T.unwords . fmap go . T.words
  where
    go "?" = "{! !}"
    go x   = x

