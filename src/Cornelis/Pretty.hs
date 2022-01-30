{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Pretty where

import Prettyprinter
import Cornelis.Types hiding (Type)
import qualified Cornelis.Types as C
import Data.Bool (bool)
import Data.Int

data HighlightGroup
  = Keyword
  | Normal
  | Type
  | Operator
  | Identifier
  | Constant
  | Number
  | Comment
  | Todo
  | String
  | Folded
  | Structure
  | Title
  | PreProc
  | Error
  | WarningMsg
  deriving (Eq, Ord, Show)

data InfoHighlight a = InfoHighlight
  { ihl_start :: (Int64, Int64)
  , ihl_end :: a
  , ihl_group :: HighlightGroup
  }
  deriving (Eq, Ord, Show, Functor)

renderWithHlGroups
    :: SimpleDocStream HighlightGroup
    -> ([InfoHighlight (Int64, Int64)], SimpleDocStream a)
renderWithHlGroups = go [] 0 0
  where
    go
      :: [InfoHighlight ()]
      -> Int64
      -> Int64
      -> SimpleDocStream HighlightGroup
      -> ([InfoHighlight (Int64, Int64)], SimpleDocStream a)
    go _ _ _ SFail = pure SFail
    go _ _ _ SEmpty = pure SEmpty
    go st r c (SChar c' sds) = SChar c' <$> go st r (c + 1) sds
    go st r c (SText n txt sds) = SText n txt <$> go st r (c + fromIntegral n) sds
    go st r c (SLine n sds) = SLine n <$> go st (r + 1) (fromIntegral n) sds
    go st r c (SAnnPush hg sds) = go (InfoHighlight (r, c) () hg : st) r c sds
    go [] r c (SAnnPop sds) = error "popping an annotation that doesn't exist"
    go (ih : ihs) r c (SAnnPop sds) = do
      sds' <- go ihs r c sds
      ([(r, c) <$ ih], sds')


prettyType :: C.Type -> Doc HighlightGroup
prettyType (C.Type ty) = annotate Type $ pretty ty


prettyGoals :: DisplayInfo -> Doc HighlightGroup
prettyGoals (AllGoalsWarnings _ _ errs _) | not $ null errs =
  annotate Error $ vcat $ fmap (pretty . getMessage) errs
prettyGoals (AllGoalsWarnings vis invis _ warns) =
  vcat $ punctuate hardline $
    [ section "Warnings" warns $ annotate WarningMsg . pretty . getMessage
    , section "Visible Goals" vis $ prettyGoal . fmap (mappend "?" . show . ip_id)
    , section "Invisible Goals" invis $ prettyGoal . fmap np_name
    ]
prettyGoals (GoalSpecific goal scoped ty) = vcat
  [ annotate Title "Goal:" <+>  prettyType ty
  , mconcat $ replicate 60 "—"
  , vcat $ fmap prettyInScope scoped
  ]
prettyGoals (DisplayError err) = annotate Error $ pretty err
prettyGoals (UnknownDisplayInfo v) = annotate Error $ pretty $ show v


section
    :: Doc HighlightGroup
    -> [a]
    -> (a -> Doc HighlightGroup)
    -> Doc HighlightGroup
section doc [] f = mempty
section doc as f = vcat $
  annotate Title (doc <> ":") : fmap f as


prettyName :: String -> Doc HighlightGroup
prettyName = annotate Identifier . pretty


prettyInScope :: InScope -> Doc HighlightGroup
prettyInScope (InScope re orig in_scope ty) =
  hsep
    [ prettyGoal $ GoalInfo orig ty
    , bool
        (pretty (replicate 6 ' ') <+> annotate Comment (parens "not in scope"))
        mempty
        in_scope
    ]

prettyGoal :: GoalInfo String -> Doc HighlightGroup
prettyGoal (GoalInfo name ty) = hsep
  [ prettyName name
  , ":"
  , prettyType ty
  ]

