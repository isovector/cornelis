{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Neovim
import Plugin
import Cornelis.Types
import Control.Concurrent (newMVar)

main :: IO ()
main = neovim defaultConfig { plugins = [cornelis] }

cornelis :: Neovim env NeovimPlugin
cornelis = do
  mvar <- liftIO $ newMVar $ CornelisState mempty
  wrapPlugin $ Plugin
    { environment = CornelisEnv mvar
    , exports = [ $(function "Cornelis" 'yo) Sync ]
    }


