{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Cornelis.Types where

import qualified Data.Map as M
import Data.Map (Map)
import Neovim.API.String (Buffer(..))
import Control.Concurrent
import Neovim
import Control.Monad.State.Class
import Data.Tuple (swap)
import Cornelis.Types.Agda (IntervalWithoutFile, Position'(..), Interval' (Interval))
import GHC.Generics
import Control.Concurrent.Chan.Unagi (InChan)
import System.IO (Handle)
import Data.Aeson

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
  , ar_message :: Response
  }

instance MonadState CornelisState (Neovim CornelisEnv) where
  state f = do
    mv <- asks ce_state
    liftIO $ modifyMVar mv $ pure . fmap swap f

data Response
  = OfType
  | CmpInType
  | CmpElim
  | JustType
  | JustSort
  | Assign
  | TypedAssign
  | PostponedCheckArgs
  | IsEmptyType
  | SizeLtSat
  | FindInstanceOF
  | PTSInstance
  | PostponedCheckFunDef
  | CheckLock
  | UsableAtMod
  | UnblockOnMeta
  | UnblockOnProblem
  | UnblockOnAll
  | UnblockOnAny
  | CompilationOk
  | Constraints
  | AllGoalsWarnings
  | Time
  | IntroNotFound
  | IntroConstructorUnknown
  | Auto
  | ModuleContents
  | SearchAbout
  | WhyInScope
  | NormalForm
  | InferredType
  | Context
  | Version
  | GoalSpecific
  | GoalOnly
  | GoalAndHave
  | GoalAndElaboration
  | HelperFunction
  | GoalType
  | CurrentGoal
  | Error
  | DisplayInfo
  | ClearHighlighting -- TokenBased
  | DoneAborting
  | DoneExiting
  | ClearRunningInfo
  | RunningInfo Int String
  | Status
    { status_checked :: Bool
    , status_showIrrelevant :: Bool
    , status_showImplicits :: Bool
    }
  | JumpToError
  | InteractionPoints [InteractionPoint]
  | GiveAction
  | MakeCase
  | SolveAll
  deriving (Eq, Ord, Show)

data InteractionPoint = InteractionPoint
  { ip_id :: Int
  , ip_interval :: IntervalWithoutFile
  }
  deriving (Eq, Ord, Show)

instance FromJSON IntervalWithoutFile where
  parseJSON = withObject "IntervalWithoutFile" $ \obj -> do
    Interval <$> obj .: "start" <*> obj .: "end"

instance FromJSON InteractionPoint where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    InteractionPoint <$> obj .: "id" <*> fmap head (obj .: "range")

instance FromJSON (Position' ()) where
  parseJSON = withObject "Position" $ \obj -> do
    Pn <$> pure () <*> obj .: "pos" <*> obj .: "line" <*> obj .: "col"

instance FromJSON Response where
  parseJSON = withObject "Response" $ \obj -> do
    obj .: "kind" >>= \case
      "ClearRunningInfo" ->
        pure ClearRunningInfo
      "ClearHighlighting" ->
        pure ClearHighlighting
      "RunningInfo" ->
        RunningInfo <$> obj .: "debugLevel" <*> obj .: "message"
      "InteractionPoints" ->
        InteractionPoints <$> obj .: "interactionPoints"
      "Status" -> do
        (obj .: "status" >>=) $ withObject "Status" $ \s ->
          Status <$> s .: "checked" <*> s .: "showIrrelevantArguments" <*> s .: "showImplicitArguments"
      (_ :: String) -> empty

