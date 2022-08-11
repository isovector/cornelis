{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cornelis.Types
  ( module Cornelis.Types
  , Interval' (..)
  , Pos' (..)
  , BufferOffset
  , LineOffset
  , Buffer
  , Window
  , Text
  , traceMX
  , HasCallStack
  ) where

import Control.Concurrent
import Control.Concurrent.Chan.Unagi (InChan)
import Control.Monad.State.Class
import Cornelis.Debug
import Cornelis.Types.Agda (IntervalWithoutFile, Interval' (..), BufferOffset, LineOffset, AgdaOffset, Pos' (..))
import Data.Aeson hiding (Error)
import Data.Generics.Labels ()
import Data.IntMap.Strict (IntMap)
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Tuple (swap)
import GHC.Generics
import GHC.Stack
import Neovim hiding (err)
import Neovim.API.Text (Buffer(..), Window)
import System.IO (Handle)
import System.Process (ProcessHandle)
import Data.Functor.Identity

deriving stock instance Ord Buffer

type Pos = Pos' LineOffset


data Agda = Agda
  { a_buffer :: Buffer
  , a_req    :: Handle
  , a_hdl    :: ProcessHandle
  }
  deriving Generic

data BufferStuff = BufferStuff
  { bs_agda_proc  :: Agda
  , bs_ips        :: IntMap (InteractionPoint Identity LineOffset)
  , bs_goto_sites :: Map Extmark DefinitionSite
  , bs_goals      :: DisplayInfo
  , bs_info_win   :: InfoBuffer
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

data SplitDirection = Vertical | Horizontal
  deriving (Eq, Ord, Show, Enum, Bounded, Read)

data CornelisConfig = CornelisConfig
  { cc_max_height :: Int64
  , cc_split_direction :: SplitDirection
  }
  deriving (Show, Generic)

data CornelisEnv = CornelisEnv
  { ce_state :: MVar CornelisState
  , ce_stream :: InChan AgdaResp
  , ce_namespace :: Int64
  , ce_config :: CornelisConfig
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
  = DisplayInfo DisplayInfo
  | ClearHighlighting -- TokenBased
  | HighlightingInfo Bool [Highlight]
  | ClearRunningInfo
  | RunningInfo Int Text
  | Status
    { status_checked :: Bool
    , status_showIrrelevant :: Bool
    , status_showImplicits :: Bool
    }
  | JumpToError FilePath BufferOffset
  | InteractionPoints [InteractionPoint Maybe AgdaOffset]
  | GiveAction Text (InteractionPoint (Const ()) AgdaOffset)
  | MakeCase MakeCase
  | SolveAll [Solution]
  | Unknown Text Value
  deriving (Eq, Ord, Show)

data DefinitionSite = DefinitionSite
  { ds_filepath :: Text
  , ds_position :: BufferOffset
  }
  deriving (Eq, Ord, Show)

instance FromJSON DefinitionSite where
  parseJSON = withObject "DefinitionSite" $ \obj ->
    DefinitionSite <$> obj .: "filepath" <*> obj .: "position"

data Highlight = Highlight
  { hl_atoms          :: [Text]
  , hl_definitionSite :: Maybe DefinitionSite
  , hl_start          :: BufferOffset
  , hl_end            :: BufferOffset
  }
  deriving (Eq, Ord, Show)

instance FromJSON Highlight where
  parseJSON = withObject "Highlight" $ \obj ->
    Highlight
      <$> obj .: "atoms"
      <*> obj .:? "definitionSite"
      <*> fmap (!! 0) (obj .: "range")
      <*> fmap (!! 1) (obj .: "range")

data MakeCase
  = RegularCase MakeCaseVariant [Text] (InteractionPoint Identity AgdaOffset)
  deriving (Eq, Ord, Show, Generic)

data MakeCaseVariant = Function | ExtendedLambda
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON)

instance FromJSON MakeCase where
  parseJSON = withObject "MakeCase" $ \obj ->
    RegularCase <$> obj .: "variant" <*> obj .: "clauses" <*> obj .: "interactionPoint"


data Solution = Solution
  { s_ip :: Int
  , s_expression :: Text
  }
  deriving (Eq, Ord, Show)

data InteractionPoint f a = InteractionPoint
  { ip_id :: Int
  , ip_interval' :: f (Interval' a)
  }
  deriving stock (Generic, Functor)

deriving instance Eq (f (Interval' a)) => Eq (InteractionPoint f a)
deriving instance Ord (f (Interval' a)) => Ord (InteractionPoint f a)
deriving instance Show (f (Interval' a)) => Show (InteractionPoint f a)

ip_interval :: InteractionPoint Identity a -> Interval' a
ip_interval (InteractionPoint _ (Identity i)) = i

sequenceInteractionPoint :: Applicative f => InteractionPoint f a -> f (InteractionPoint Identity a)
sequenceInteractionPoint (InteractionPoint n f) = InteractionPoint <$> pure n <*> fmap Identity f


data NamedPoint = NamedPoint
  { np_name :: Text
  , np_interval :: Maybe (IntervalWithoutFile)
  }
  deriving (Eq, Ord, Show)

instance FromJSON IntervalWithoutFile where
  parseJSON = withObject "IntervalWithoutFile" $ \obj -> do
    Interval <$> obj .: "start" <*> obj .: "end"

instance FromJSON (Interval' a) => FromJSON (InteractionPoint Maybe a) where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    InteractionPoint <$> obj .: "id" <*> fmap listToMaybe (obj .: "range")

instance FromJSON (Interval' a) => FromJSON (InteractionPoint Identity a) where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    InteractionPoint <$> obj .: "id" <*> fmap head (obj .: "range")

instance FromJSON (Interval' a) => FromJSON (InteractionPoint (Const ()) a) where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    InteractionPoint <$> obj .: "id" <*> pure (Const ())

instance FromJSON NamedPoint where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    NamedPoint <$> obj .: "name" <*> fmap listToMaybe (obj .: "range")

instance FromJSON b => FromJSON (Pos' b) where
  parseJSON = withObject "Position" $ \obj -> do
    Pos <$> obj .: "line" <*> obj .: "col"

instance FromJSON Solution where
  parseJSON = withObject "Solution" $ \obj ->
    Solution <$> obj .: "interactionPoint" <*> obj .: "expression"

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
      (_ :: Text) -> empty

newtype Type = Type Text
  deriving newtype (Eq, Ord, Show, FromJSON)

data InScope = InScope
  { is_refied_name :: Text
  , is_original_name :: Text
  , is_in_scope :: Bool
  , is_type :: Type
  }
  deriving (Eq, Ord, Show)

instance FromJSON InScope where
  parseJSON = withObject "InScope" $ \obj ->
    InScope <$> obj .: "reifiedName" <*> obj .: "originalName" <*> obj .: "inScope" <*> obj .: "binding"

newtype Message = Message { getMessage :: Text }
  deriving (Eq, Ord, Show)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \obj ->
    Message <$> obj .: "message"


data DisplayInfo
  = AllGoalsWarnings
      { di_all_visible :: [GoalInfo (InteractionPoint Identity AgdaOffset)]
      , di_all_invisible :: [GoalInfo NamedPoint]
      , di_errors :: [Message]
      , di_warnings :: [Message]
      }
  | GoalSpecific
      { di_ips :: InteractionPoint Identity AgdaOffset
      , di_in_scope :: [InScope]
      , di_type :: Type
      , di_type_aux :: Maybe Type
      , di_boundary :: Maybe [Text]
      , di_output_forms :: Maybe [Text]
      }
  | HelperFunction Text
  | DisplayError Text
  | WhyInScope Text
  | NormalForm Text
  | UnknownDisplayInfo Value
  deriving (Eq, Ord, Show, Generic)

data TypeAux = TypeAux
  { ta_expr :: Type
  }

instance FromJSON TypeAux where
  parseJSON = withObject "TypeAux" $ \obj ->
    (TypeAux . Type) <$> obj .: "expr"

instance FromJSON DisplayInfo where
  parseJSON v = flip (withObject "DisplayInfo") v $ \obj ->
    obj .: "kind" >>= \case
      "AllGoalsWarnings" ->
        AllGoalsWarnings
          <$> obj .: "visibleGoals"
          <*> obj .: "invisibleGoals"
          <*> (obj .: "errors"   <|> fmap pure (obj .: "errors"))
          <*> (obj .: "warnings" <|> fmap pure (obj .: "warnings"))
      "Error" ->
        obj .: "error" >>= \err ->
          DisplayError <$> err .: "message"
      "WhyInScope" ->
        WhyInScope <$> obj .: "message"
      "NormalForm" ->
        NormalForm <$> obj .: "expr"
      "GoalSpecific" ->
        (obj .: "goalInfo") >>= \info ->
          (info .: "kind") >>= \case
            "HelperFunction" ->
              HelperFunction <$> info .: "signature"
            "GoalType" ->
              GoalSpecific
                <$> obj .: "interactionPoint"
                <*> info .: "entries"
                <*> info .: "type"
                <*> (fmap (fmap ta_expr) (info .: "typeAux") <|> pure Nothing)
                <*> info .:? "boundary"
                <*> info .:? "outputForms"
            (_ :: Text) ->
              pure $ UnknownDisplayInfo v
      (_ :: Text) -> pure $ UnknownDisplayInfo v

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
          Status <$> s .: "checked"
                  <*> (s .: "showIrrelevantArguments"<|> pure False)
                  <*> s .: "showImplicitArguments"
      (_ :: Text) -> Unknown <$> obj .: "kind" <*> pure v

newtype Extmark = Extmark Int64
  deriving (Eq, Ord, Show)

data ExtmarkStuff = ExtmarkStuff
  { es_mark     :: Extmark
  , es_hlgroup  :: Text
  , es_interval :: Interval' LineOffset
  }
  deriving (Eq, Ord, Show)

