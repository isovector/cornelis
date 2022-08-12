{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Config where

import           Cornelis.Types
import           Cornelis.Utils (objectToInt, objectToText)
import           Data.Functor.Compose
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Neovim
import           Neovim.API.Text


getVar :: Text -> Neovim env (Maybe Object)
getVar v
  = catchNeovimException (fmap Just $ vim_get_var v)
  $ const
  $ pure Nothing

-- Maybe we should batch these to avoid making too many RPC requests and/or display
-- a deprecation message?
getVarWithAlternatives :: [Text] -> Neovim env (Maybe Object)
getVarWithAlternatives [] = pure Nothing
getVarWithAlternatives (v : vs) =
  do
    t <- getVar v
    case t of
      Just _ -> return t
      Nothing -> getVarWithAlternatives vs


traverseCompose :: (Functor f, Monad g) => (a -> g b) -> Compose f g a -> Compose f g b
traverseCompose f (Compose fga) = Compose $ fmap (f =<<) fga


getConfig :: Neovim env CornelisConfig
getConfig
  = CornelisConfig
    <$> fmap (fromMaybe 31 . (objectToInt =<<))
        (getVar "cornelis_max_size")
    <*> fmap (fromMaybe Horizontal . (readSplitLocation =<<) . fmap T.unpack . (objectToText =<<))
        (getVarWithAlternatives ["cornelis_split_location", "cornelis_split_direction"])

