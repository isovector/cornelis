{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveAnyClass #-}

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
import Cornelis.Types.Agda (IntervalWithoutFile, Position'(..), Interval' (Interval), BufferOffset, LineOffset)
import GHC.Generics
import Control.Concurrent.Chan.Unagi (InChan)
import System.IO (Handle)
import Data.Aeson hiding (Error)
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
  , bs_info_win  :: InfoBuffer
  }
  deriving Generic

newtype InfoBuffer = InfoBuffer
  { iw_buffer :: Buffer
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
  = CmpInType
  | CmpElim
  | JustType
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
  | GoalOnly
  | GoalAndHave
  | GoalAndElaboration
  | HelperFunction
  | GoalType
  | CurrentGoal
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
  | JumpToError FilePath BufferOffset
  | InteractionPoints [InteractionPoint]
  | GiveAction String InteractionPoint
  | MakeCase MakeCase
  | SolveAll [Solution]
  | Unknown String Value
  deriving (Eq, Ord, Show)

data Highlight = Highlight
  { hl_atoms :: [String]
  -- , hl_definitionSite :: (FilePath, Position')
  , hl_start :: BufferOffset
  , hl_end :: BufferOffset
  }
  deriving (Eq, Ord, Show)

data MakeCase
  = RegularCase MakeCaseVariant [String] InteractionPoint
  deriving (Eq, Ord, Show)

data MakeCaseVariant = Function | ExtendedLambda
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON)

instance FromJSON MakeCase where
  parseJSON = withObject "MakeCase" $ \obj ->
    RegularCase <$> obj .: "variant" <*> obj .: "clauses" <*> obj .: "interactionPoint"


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

data NamedPoint = NamedPoint
  { np_name :: String
  , np_interval :: IntervalWithoutFile
  }
  deriving (Eq, Ord, Show)

instance FromJSON IntervalWithoutFile where
  parseJSON = withObject "IntervalWithoutFile" $ \obj -> do
    Interval <$> obj .: "start" <*> obj .: "end"

instance FromJSON InteractionPoint where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    InteractionPoint <$> obj .: "id" <*> fmap head (obj .: "range")

instance FromJSON NamedPoint where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    NamedPoint <$> obj .: "name" <*> fmap head (obj .: "range")

instance FromJSON b => FromJSON (Position' b ()) where
  parseJSON = withObject "Position" $ \obj -> do
    Pn <$> pure () <*> obj .: "pos" <*> obj .: "line" <*> obj .: "col"

instance FromJSON Solution where
  parseJSON = withObject "Solution" $ \obj ->
    Solution <$> obj .: "interactionPoint" <*> obj .: "expression"

instance FromJSON Highlight where
  parseJSON = withObject "Highlight" $ \obj ->
    Highlight <$> obj .: "atoms" <*> fmap (!! 0) (obj .: "range") <*> fmap (!! 1) (obj .: "range")

data GoalInfo a = GoalInfo
  { gi_ip :: a
  , gi_type :: Type
  }
  deriving (Eq, Ord, Show, Functor)

instance FromJSON a => FromJSON (GoalInfo a) where
  parseJSON = withObject "GoalInfo" $ \obj ->
    (obj .: "kind") >>= \case
      "OfType" -> GoalInfo <$> obj .: "constraintObj" <*> obj .: "type"
      "JustSort" -> GoalInfo <$> obj .: "constraintObj" <*> pure (Type "Sort")
      (_ :: String) -> empty

newtype Type = Type String
  deriving newtype (Eq, Ord, Show, FromJSON)

data InScope = InScope
  { is_refied_name :: String
  , is_original_name :: String
  , is_in_scope :: Bool
  , is_type :: Type
  }
  deriving (Eq, Ord, Show)

instance FromJSON InScope where
  parseJSON = withObject "InScope" $ \obj ->
    InScope <$> obj .: "reifiedName" <*> obj .: "originalName" <*> obj .: "inScope" <*> obj .: "binding"

newtype Message = Message { getMessage :: String }
  deriving (Eq, Ord, Show)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \obj ->
    Message <$> obj .: "message"


data DisplayInfo
  = AllGoalsWarnings
      { di_all_visible :: [GoalInfo InteractionPoint]
      , di_all_invisible :: [GoalInfo NamedPoint]
      , di_errors :: [Message]
      , di_warnings :: [Message]
      }
  | GoalSpecific InteractionPoint [InScope] Type
  | DisplayError String
  | UnknownDisplayInfo Value
  deriving (Eq, Ord, Show, Generic)

instance FromJSON DisplayInfo where
  parseJSON v = flip (withObject "DisplayInfo") v $ \obj ->
    obj .: "kind" >>= \case
      "AllGoalsWarnings" ->
        AllGoalsWarnings <$> obj .: "visibleGoals" <*> obj .: "invisibleGoals" <*> obj .: "errors" <*> obj .: "warnings"
      "Error" ->
        obj .: "error" >>= \err ->
          DisplayError <$> err .: "message"
      "GoalSpecific" ->
        (obj .: "goalInfo") >>= \info ->
          GoalSpecific <$> obj .: "interactionPoint" <*> info .: "entries" <*> info .: "type"
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
      "GiveAction" ->
        (obj .: "giveResult") >>= \result ->
          GiveAction <$> result .: "str" <*> obj .: "interactionPoint"
      "DisplayInfo" ->
        DisplayInfo <$> obj .: "info"
      "JumpToError" ->
        JumpToError <$> obj .: "filepath" <*> obj .: "position"
      "Status" -> do
        (obj .: "status" >>=) $ withObject "Status" $ \s ->
          Status <$> s .: "checked" <*> s .: "showIrrelevantArguments" <*> s .: "showImplicitArguments"
      (_ :: String) -> Unknown <$> obj .: "kind" <*> pure v

