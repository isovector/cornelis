{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}

module Cornelis.Types.Agda where

import           Control.Monad (liftM4)
import           Control.Monad (mplus, liftM2)
import           Control.Monad.Except (ExceptT, throwError)
import           Control.Monad.State.Strict (StateT, runStateT, put, get)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Aeson (FromJSON)
import           Data.Bifunctor
import           Data.Data
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Int
import qualified Data.List as List
import           Data.Maybe (listToMaybe)
import           Data.Sequence
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           GHC.Generics
import           GHC.Show (showSpace)
import           System.FilePath

------------------------------------------------------------------------------
-- | Line numbers are always 1-indexed
newtype LineNumber = LineNumber { getOneIndexedLineNumber :: Int32 }
  deriving stock Data
  deriving newtype (Eq, Ord, Show, Read, FromJSON)

data OffsetType = Line | File | OneIndexed

newtype Offset (a :: OffsetType) = Offset Int32
  deriving stock Data
  deriving newtype (Eq, Ord, Show, Read, FromJSON)

type BufferOffset = Offset 'File
type LineOffset = Offset 'Line
type AgdaOffset = Offset 'OneIndexed


data Rewrite =  AsIs | Instantiated | HeadNormal | Simplified | Normalised
    deriving (Show, Read, Eq, Ord)


data ComputeMode = DefaultCompute | HeadCompute | IgnoreAbstract | UseShowInstance
  deriving (Show, Read, Eq)

data UseForce
  = WithForce     -- ^ Ignore additional checks, like termination/positivity...
  | WithoutForce  -- ^ Don't ignore any checks.
  deriving (Eq, Read, Show)


importantPart :: Position' b a -> (a, BufferOffset)
importantPart p = (srcFile p, posPos p)

instance Eq a => Eq (Position' b a) where
  (==) = (==) `on` importantPart

instance Ord a => Ord (Position' b a) where
  compare = compare `on` importantPart


newtype InteractionId = InteractionId { interactionId :: Int }
  deriving newtype
           ( Eq
           , Ord
           , Show
           , Read
           , Num
           , Integral
           , Real
           , Enum
           )


-- | IOTCM commands.

type Command = Command' IOTCM


data Command' a
  = Command !a
    -- ^ A command.
  | Done
    -- ^ Stop processing commands.
  | Error String
    -- ^ An error message for a command that could not be parsed.
  deriving Show

data Range' a
  = NoRange
  | Range !a (Seq IntervalWithoutFile)
  deriving
    (Data, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Show a =>
         Show (Range' a) where
  showsPrec _ NoRange
    = showString "noRange"
  showsPrec
    a_a1hOk
    (Cornelis.Types.Agda.Range b1_a1hOl b2_a1hOm)
    = showParen
        (a_a1hOk >= 11)
        ((.)
           (showString "Range ")
           ((.)
              (showsPrec 11 b1_a1hOl)
              ((.)
                 showSpace (showsPrec 11 b2_a1hOm))))

data Interval' b a = Interval { iStart, iEnd :: !(Position' b  a) }
  deriving (Show, Data, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Bifunctor Interval' where
  bimap fab fcd (Interval a b) = Interval (bimap fab fcd a) (bimap fab fcd b)

type Interval            = Interval' AgdaOffset SrcFile
type IntervalWithoutFile = Interval' AgdaOffset ()

type SrcFile = Maybe AbsolutePath

data Position' b a = Pn
  { srcFile :: !a
    -- ^ File.
  , posPos  :: !BufferOffset
    -- ^ Position, counting from 1.
  , posLine :: !LineNumber
    -- ^ Line number, counting from 1.
  , posCol  :: !b
    -- ^ Column number, counting from 1.
  }
  deriving (Show, Data, Functor, Foldable, Traversable, Generic)

instance Bifunctor Position' where
  bimap fab fcd (Pn c off ln a) = Pn (fcd c) off ln $ fab a


newtype AbsolutePath = AbsolutePath { textPath :: String }
  deriving (Show, Eq, Ord, Data)



type Range = Range' SrcFile

type IOTCM = IOTCM' Range
data IOTCM' range
    = IOTCM
        Text
         -- -^ The current file. If this file does not match
         -- 'theCurrentFile, and the 'Interaction' is not
         -- \"independent\", then an error is raised.
        HighlightingLevel
        HighlightingMethod
        (Interaction' range)
         -- -^ What to do
            deriving (Show, Read, Functor, Foldable, Traversable)




data Interaction' range
    -- | @cmd_load m argv@ loads the module in file @m@, using
    -- @argv@ as the command-line options.
  = Cmd_load            Text [String]

  | Cmd_constraints

    -- | Show unsolved metas. If there are no unsolved metas but unsolved constraints
    -- show those instead.
  | Cmd_metas Rewrite

    -- | Shows all the top-level names in the given module, along with
    -- their types. Uses the top-level scope.
  | Cmd_show_module_contents_toplevel
                        Rewrite
                        String

    -- | Shows all the top-level names in scope which mention all the given
    -- identifiers in their type.
  | Cmd_search_about_toplevel Rewrite String

    -- | Solve (all goals / the goal at point) whose values are determined by
    -- the constraints.
  | Cmd_solveAll Rewrite
  | Cmd_solveOne Rewrite InteractionId range String

    -- | Solve (all goals / the goal at point) by using Auto.
  | Cmd_autoOne            InteractionId range String
  | Cmd_autoAll

    -- | Parse the given expression (as if it were defined at the
    -- top-level of the current module) and infer its type.
  | Cmd_infer_toplevel  Rewrite  -- Normalise the type?
                        String


    -- | Parse and type check the given expression (as if it were defined
    -- at the top-level of the current module) and normalise it.
  | Cmd_compute_toplevel ComputeMode
                         String

    ------------------------------------------------------------------------
    -- Syntax highlighting

    -- | @cmd_load_highlighting_info source@ loads syntax highlighting
    -- information for the module in @source@, and asks Emacs to apply
    -- highlighting info from this file.
    --
    -- If the module does not exist, or its module name is malformed or
    -- cannot be determined, or the module has not already been visited,
    -- or the cached info is out of date, then no highlighting information
    -- is printed.
    --
    -- This command is used to load syntax highlighting information when a
    -- new file is opened, and it would probably be annoying if jumping to
    -- the definition of an identifier reset the proof state, so this
    -- command tries not to do that. One result of this is that the
    -- command uses the current include directories, whatever they happen
    -- to be.
  | Cmd_load_highlighting_info FilePath

    -- | Tells Agda to compute token-based highlighting information
    -- for the file.
    --
    -- This command works even if the file's module name does not
    -- match its location in the file system, or if the file is not
    -- scope-correct. Furthermore no file names are put in the
    -- generated output. Thus it is fine to put source code into a
    -- temporary file before calling this command. However, the file
    -- extension should be correct.
    --
    -- If the second argument is 'Remove', then the (presumably
    -- temporary) file is removed after it has been read.
  | Cmd_tokenHighlighting FilePath Remove

    -- | Tells Agda to compute highlighting information for the expression just
    --   spliced into an interaction point.
  | Cmd_highlight InteractionId range String

    ------------------------------------------------------------------------
    -- Implicit arguments

    -- | Tells Agda whether or not to show implicit arguments.
  | ShowImplicitArgs    Bool -- Show them?


    -- | Toggle display of implicit arguments.
  | ToggleImplicitArgs

    ------------------------------------------------------------------------
    -- Irrelevant arguments

    -- | Tells Agda whether or not to show irrelevant arguments.
  | ShowIrrelevantArgs    Bool -- Show them?


    -- | Toggle display of irrelevant arguments.
  | ToggleIrrelevantArgs

    ------------------------------------------------------------------------
    -- | Goal commands
    --
    -- If the range is 'noRange', then the string comes from the
    -- minibuffer rather than the goal.

  | Cmd_give            UseForce InteractionId range String

  | Cmd_refine          InteractionId range String

  | Cmd_intro           Bool InteractionId range String

  | Cmd_refine_or_intro Bool InteractionId range String

  | Cmd_context         Rewrite InteractionId range String

  | Cmd_helper_function Rewrite InteractionId range String

  | Cmd_infer           Rewrite InteractionId range String

  | Cmd_goal_type       Rewrite InteractionId range String

  -- | Grabs the current goal's type and checks the expression in the hole
  -- against it. Returns the elaborated term.
  | Cmd_elaborate_give
                        Rewrite InteractionId range String

    -- | Displays the current goal and context.
  | Cmd_goal_type_context Rewrite InteractionId range String

    -- | Displays the current goal and context /and/ infers the type of an
    -- expression.
  | Cmd_goal_type_context_infer
                        Rewrite InteractionId range String

  -- | Grabs the current goal's type and checks the expression in the hole
  -- against it.
  | Cmd_goal_type_context_check
                        Rewrite InteractionId range String

    -- | Shows all the top-level names in the given module, along with
    -- their types. Uses the scope of the given goal.
  | Cmd_show_module_contents
                        Rewrite InteractionId range String

  | Cmd_make_case       InteractionId range String

  | Cmd_compute         ComputeMode
                        InteractionId range String

  | Cmd_why_in_scope    InteractionId range String
  | Cmd_why_in_scope_toplevel String
    -- | Displays version of the running Agda
  | Cmd_show_version
  | Cmd_abort
    -- ^ Abort the current computation.
    --
    -- Does nothing if no computation is in progress.
  | Cmd_exit
    -- ^ Exit the program.
        deriving (Show, Read, Functor, Foldable, Traversable)

type Interaction = Interaction' Range

data HighlightingLevel
  = None
  | NonInteractive
  | Interactive
    -- ^ This includes both non-interactive highlighting and
    -- interactive highlighting of the expression that is currently
    -- being type-checked.
    deriving (Eq, Ord, Show, Read, Data, Generic)

data HighlightingMethod
  = Direct
    -- ^ Via stdout.
  | Indirect
    -- ^ Both via files and via stdout.
    deriving (Eq, Show, Read, Data, Generic)


data Remove
  = Remove
  | Keep
  deriving (Show, Read)


instance Read a => Read (Range' a) where
    readsPrec = parseToReadsPrec $
      (exact "intervalsToRange" >>
       liftM2 intervalsToRange readParse readParse)
        `mplus`
      (exact "noRange" >> return noRange)

instance (Read b, Read a) => Read (Interval' b a) where
    readsPrec = parseToReadsPrec $ do
        exact "Interval"
        liftM2 Interval readParse readParse

instance Read AbsolutePath where
    readsPrec = parseToReadsPrec $ do
        exact "mkAbsolute"
        fmap mkAbsolute readParse


mkAbsolute :: FilePath -> AbsolutePath
mkAbsolute f
  | isAbsolute f =
      AbsolutePath $ dropTrailingPathSeparator $ normalise f
        -- normalize does not resolve symlinks
  | otherwise    = error "impossible"


instance (Read b, Read a) => Read (Position' b a) where
    readsPrec = parseToReadsPrec $ do
        exact "Pn"
        liftM4 Pn readParse readParse readParse readParse

type Parse a = ExceptT String (StateT String Identity) a


readsToParse :: String -> (String -> Maybe (a, String)) -> Parse a
readsToParse s f = do
  st <- lift get
  case f st of
    Nothing -> throwError s
    Just (a, st') -> do
        lift $ put st'
        return a



parseToReadsPrec :: Parse a -> Int -> String -> [(a, String)]
parseToReadsPrec p _ s = case runIdentity . flip runStateT s . runExceptT $ parens' p of
  (Right a, s') -> [(a,s')]
  _            -> []

exact :: String -> Parse ()
exact s = readsToParse (show s) $ fmap ((),) . List.stripPrefix s . dropWhile (==' ')

readParse :: Read a => Parse a
readParse = readsToParse "read failed" $ listToMaybe . reads

parens' :: Parse a -> Parse a
parens' p = do
    exact "("
    x <- p
    exact ")"
    return x
  `mplus`
    p

noRange :: Range' a
noRange = NoRange



-- | Converts a file name and an interval to a range.
intervalToRange :: a -> IntervalWithoutFile -> Range' a
intervalToRange f i = Range f (Seq.singleton i)


-- | Turns a file name plus a list of intervals into a range.
--
-- Precondition: 'consecutiveAndSeparated'.
intervalsToRange :: a -> [IntervalWithoutFile] -> Range' a
intervalsToRange _ [] = NoRange
intervalsToRange f is = Range f (Seq.fromList is)

