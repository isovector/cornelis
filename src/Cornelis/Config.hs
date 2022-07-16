{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Config where

import Cornelis.Types
import Cornelis.Utils (objectToInt)
import Data.Functor.Compose
import Data.Maybe (fromMaybe)
import Neovim
import Neovim.API.Text


getVar :: Text -> Compose (Neovim env) Maybe Object
getVar v
  = Compose
  $ catchNeovimException (fmap Just $ vim_get_var v)
  $ const
  $ pure Nothing


traverseCompose :: (Functor f, Monad g) => (a -> g b) -> Compose f g a -> Compose f g b
traverseCompose f (Compose fga) = Compose $ fmap (f =<<) fga


getConfig :: Neovim env CornelisConfig
getConfig
  = fmap (fromMaybe defConfig)
  $ getCompose
  $ CornelisConfig
    <$> traverseCompose objectToInt (getVar "cornelis_max_size")

defConfig :: CornelisConfig
defConfig = CornelisConfig
  { cc_max_height = 50
  }

