{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cornelis.Config where

import           Cornelis.Types
import           Cornelis.Utils (objectToInt, objectToText, objectToBool)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Neovim
import           Neovim.API.Text
import           Control.Monad ((<=<))


------------------------------------------------------------------------------
-- | Attempt to get a variable from vim.
getVar :: Text -> Neovim env (Maybe Object)
getVar v
  = catchNeovimException (Just <$> vim_get_var v)
  $ const
  $ pure Nothing


------------------------------------------------------------------------------
-- | Get the first variable from vim that succeeds. Useful for variable names
-- that have changed over time.
getVarWithAlternatives :: [Text] -> Neovim env (Maybe Object)
getVarWithAlternatives = fmap getFirst . foldMap (fmap First . getVar)


------------------------------------------------------------------------------
-- | Build a 'CornelisConfig' from .vimrc
getConfig :: Neovim env CornelisConfig
getConfig = do
  cc_max_height <-
    fmap
      (fromMaybe 31 . (objectToInt =<<))
      (getVar "cornelis_max_size")
  cc_max_width <-
    fmap
      (fromMaybe 31 . (objectToInt =<<))
      (getVar "cornelis_max_width")
  cc_split_location <-
    fromMaybe Horizontal . (>>= (readSplitLocation . T.unpack <=< objectToText)) <$>
      getVarWithAlternatives ["cornelis_split_location", "cornelis_split_direction"]
  cc_sync_load <-
    (/= (0 :: Int)) . fromMaybe 0 . (objectToInt =<<) <$> getVar "cornelis_sync_load"
  pure CornelisConfig {..}
