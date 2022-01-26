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
import Control.Concurrent.Chan.Unagi (InChan)
import System.IO (Handle)

deriving stock instance Ord Buffer

data Agda = Agda
  { a_buffer :: Buffer
  , a_req  :: Handle
  }

data CornelisState = CornelisState
  { cs_procs :: Map Buffer Agda
  }
  deriving Generic

data CornelisEnv = CornelisEnv
  { ce_state :: MVar CornelisState
  , ce_stream :: InChan AgdaResp
  }

data AgdaResp = AgdaResp
  { ar_buffer :: Buffer
  , ar_message :: String
  }

instance MonadState CornelisState (Neovim CornelisEnv) where
  state f = do
    mv <- asks ce_state
    liftIO $ modifyMVar mv $ pure . fmap swap f

