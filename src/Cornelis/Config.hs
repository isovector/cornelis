{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Config where

import           Cornelis.Types
import           Cornelis.Utils (objectToInt, objectToText)
import           Data.Functor.Compose
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Neovim
import           Neovim.API.Text
import           Text.Read (readMaybe)


getVar :: Text -> Neovim env (Maybe Object)
getVar v
  = catchNeovimException (fmap Just $ vim_get_var v)
  $ const
  $ pure Nothing


traverseCompose :: (Functor f, Monad g) => (a -> g b) -> Compose f g a -> Compose f g b
traverseCompose f (Compose fga) = Compose $ fmap (f =<<) fga


getConfig :: Neovim env CornelisConfig
getConfig
  = CornelisConfig
    <$> fmap (fromMaybe 31 . (objectToInt =<<))
        (getVar "cornelis_max_size")
    <*> fmap (fromMaybe Horizontal . (readMaybe =<<) . fmap T.unpack . (objectToText =<<))
        (getVar "cornelis_split_direction")

