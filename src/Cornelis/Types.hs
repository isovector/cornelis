{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Cornelis.Types
  ( module Cornelis.Types
  , Buffer
  , Window
  ) where

import qualified Data.Map as M
import Data.Map (Map)
import Neovim.API.String (Buffer(..), Window)
import Control.Concurrent
import Neovim
import Control.Monad.State.Class
import Data.Tuple (swap)
import Cornelis.Types.Agda (IntervalWithoutFile, Position'(..), Interval' (Interval))
import GHC.Generics
import Control.Concurrent.Chan.Unagi (InChan)
import System.IO (Handle)
import Data.Aeson
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

deriving stock instance Ord Buffer

data Agda = Agda
  { a_buffer :: Buffer
  , a_req  :: Handle
  }
  deriving Generic

data BufferStuff = BufferStuff
  { bs_agda_proc :: Agda
  , bs_ips       :: IntMap InteractionPoint
  , bs_goals     :: DisplayInfo
  , bs_info_win  :: Maybe InfoWin
  }
  deriving Generic

data InfoWin = InfoWin
  { iw_window :: Window
  , iw_buffer :: Buffer
  }
  deriving Generic

data CornelisState = CornelisState
  { cs_buffers :: Map Buffer BufferStuff
  }
  deriving Generic

data CornelisEnv = CornelisEnv
  { ce_state :: MVar CornelisState
  , ce_stream :: InChan AgdaResp
  , ce_namespace :: Int64
  }
  deriving Generic

data AgdaResp = AgdaResp
  { ar_buffer :: Buffer
  , ar_message :: Response
  }
  deriving Generic

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
  | DisplayInfo DisplayInfo
  | ClearHighlighting -- TokenBased
  | HighlightingInfo Bool [Highlight]
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
  | MakeCase MakeCase
  | SolveAll [Solution]
  | Unknown String Value
  deriving (Eq, Ord, Show)

data Highlight = Highlight
  { hl_atoms :: [String]
  -- , hl_definitionSite :: (FilePath, Position')
  , hl_start :: Int
  , hl_end :: Int
  }
  deriving (Eq, Ord, Show)

data MakeCase
  = MakeFunctionCase [String] InteractionPoint
  deriving (Eq, Ord, Show)

instance FromJSON MakeCase where
  parseJSON = withObject "MakeCase" $ \obj ->
    MakeFunctionCase <$> obj .: "clauses" <*> obj .: "interactionPoint"


data Solution = Solution
  { s_ip :: Int
  , s_expression :: String
  }
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

instance FromJSON Solution where
  parseJSON = withObject "Solution" $ \obj ->
    Solution <$> obj .: "interactionPoint" <*> obj .: "expression"

instance FromJSON Highlight where
  parseJSON = withObject "Highlight" $ \obj ->
    Highlight <$> obj .: "atoms" <*> fmap (!! 0) (obj .: "range") <*> fmap (!! 1) (obj .: "range")

data GoalInfo = GoalInfo
  { gi_ip :: InteractionPoint
  , gi_type :: String
  }
  deriving (Eq, Ord, Show)

instance FromJSON GoalInfo where
  parseJSON = withObject "GoalInfo" $ \obj ->
    -- TODO(sandy): This thing also has a kind, that always looks like it is
    -- "OfType", but who knows
    GoalInfo <$> obj .: "constraintObj" <*> obj .: "type"

data DisplayInfo
  = AllGoalsWarnings
      { di_all_visible :: [GoalInfo]
      , di_all_invisible :: [GoalInfo]
      }
  | UnknownDisplayInfo Value
  deriving (Eq, Ord, Show)

instance FromJSON DisplayInfo where
  parseJSON v = flip (withObject "DisplayInfo") v $ \obj ->
    obj .: "kind" >>= \case
      "AllGoalsWarnings" ->
        AllGoalsWarnings <$> obj .: "visibleGoals" <*> obj .: "invisibleGoals"
      (_ :: String) -> pure $ UnknownDisplayInfo v

instance FromJSON Response where
  parseJSON v = flip (withObject "Response") v $ \obj -> do
    obj .: "kind" >>= \case
      "MakeCase" ->
        MakeCase <$> parseJSON v
      "ClearRunningInfo" ->
        pure ClearRunningInfo
      "HighlightingInfo" ->
        (obj .: "info") >>= \info ->
          HighlightingInfo <$> info .: "remove" <*> info .: "payload"
      "ClearHighlighting" ->
        pure ClearHighlighting
      "RunningInfo" ->
        RunningInfo <$> obj .: "debugLevel" <*> obj .: "message"
      "InteractionPoints" ->
        InteractionPoints <$> obj .: "interactionPoints"
      "SolveAll" ->
        SolveAll <$> obj .: "solutions"
      "DisplayInfo" ->
        DisplayInfo <$> obj .: "info"
      "Status" -> do
        (obj .: "status" >>=) $ withObject "Status" $ \s ->
          Status <$> s .: "checked" <*> s .: "showIrrelevantArguments" <*> s .: "showImplicitArguments"
      (_ :: String) -> Unknown <$> obj .: "kind" <*> pure v

