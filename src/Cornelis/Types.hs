{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Cornelis.Types where

import qualified Data.Map as M
import Data.Map (Map)
import Neovim.API.String (Buffer(..))
import Control.Concurrent
import Neovim
import Control.Monad.State.Class
import Data.Tuple (swap)
import GHC.Generics

deriving stock instance Ord Buffer

data CornelisState = CornelisState
  { cs_procs :: Map Buffer (IO ())
  }
  deriving Generic

newtype CornelisEnv = CornelisEnv
  { ce_state :: MVar CornelisState
  }

instance MonadState CornelisState (Neovim CornelisEnv) where
  state f = do
    mv <- asks ce_state
    liftIO $ modifyMVar mv $ pure . fmap swap f

