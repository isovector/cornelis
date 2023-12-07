-- | Maintain a Diff between the text that Agda loaded most recently
-- and the current Vim buffer. There is one such diff for every buffer,
-- indexed by 'BufferNum'.
--
-- This exports three functions:
-- - 'resetDiff' empties the diff when reloading Agda
-- - 'recordUpdate' adds a buffer update to the diff.
-- - 'translateInterval' applies the Diff to an interval coming from Agda,
--   turning it into an interval for the current buffer.
module Cornelis.Diff
  ( resetDiff
  , translateInterval
  , recordUpdate
  , Replace(..)
  , Colline(..)
  , Vallee(..)
  ) where

import Data.IORef (atomicModifyIORef')
import qualified Data.Map as Map
import Data.Tuple (swap)
import DiffLoc (Replace(..), Colline(..), Vallee(..))
import qualified DiffLoc as D
import Neovim
import Cornelis.Offsets
import Cornelis.Types

-- | A general function for modifying Diffs, shared between the three functions below.
modifyDiff :: BufferNum -> (Diff0 -> (Diff0, a)) -> Neovim CornelisEnv a
modifyDiff buf f = do
  csRef <- asks ce_state
  liftIO $ atomicModifyIORef' csRef $ \cs ->
    let (a, ds') = Map.alterF (fmap Just . swap . alter) buf (cs_diff cs)
    in (cs { cs_diff = ds' }, a)
  where
    alter Nothing = f D.emptyDiff
    alter (Just d) = f d

-- These three functions are essentially monadic wrappers, using 'modifyDiff',
-- around the corresponding diff-loc functions:
--
-- @
-- 'D.emptyDiff' :: Diff0
-- 'D.addDiff' :: D.Replace DPos -> Diff0 -> Diff0
-- 'D.mapDiff' :: Diff0 -> D.Interval DPos -> Maybe (D.Interval DPos)
-- @
--
-- This relies on instances of 'D.Amor' and 'D.Origin' implemented in 'Cornelis.Offsets'.

-- | Reset the diff to an empty diff.
resetDiff :: BufferNum -> Neovim CornelisEnv ()
resetDiff buf = modifyDiff buf $ const (D.emptyDiff, ())

-- | Add a buffer update (insertion or deletion) to the diff.
-- The buffer update event coming from Vim is structured exactly how the diff-loc
-- library expects it.
recordUpdate :: BufferNum -> D.Replace DPos -> Neovim CornelisEnv ()
recordUpdate buf r = modifyDiff buf $ \d -> (D.addDiff r d, ())

-- | Given an interval coming from Agda (a pair of start and end positions),
-- find the corresponding interval in the current buffer.
-- If an edit touches the interval, return Nothing.
translateInterval :: BufferNum -> Interval VimPos -> Neovim CornelisEnv (Maybe (Interval VimPos))
translateInterval buf sp = modifyDiff buf $ \d -> (d, translate d)
  where
    translate :: Diff0 -> Maybe (Interval VimPos)
    translate d = fmap toInterval . D.mapDiff d =<< fromInterval sp

-- | Convert a Cornelis interval (pair of positions) to a diff-loc interval
-- (start position and a vector towards the end position). This is Nothing
-- if the end precedes the start.
fromInterval :: Interval VimPos -> Maybe (D.Interval DPos)
fromInterval (Interval p1 p2) = (q1 D.:..) <$> (q2 D..-.? q1)
  where
    q1 = fromVimPos p1
    q2 = fromVimPos p2

-- | Inverse of 'fromInterval'.
toInterval :: D.Interval DPos -> Interval VimPos
toInterval (q D.:.. v) = Interval (toVimPos q) (toVimPos (q D..+ v))

-- | Convert from Cornelis positions to diff-loc positions.
fromVimPos :: VimPos -> DPos
fromVimPos (Pos l c) = Colline l c

toVimPos :: DPos -> VimPos
toVimPos (Colline l c) = Pos l c
