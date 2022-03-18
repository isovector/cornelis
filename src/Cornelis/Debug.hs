module Cornelis.Debug where

import Data.IORef
import Neovim
import Neovim.API.String (vim_report_error)
import System.IO.Unsafe


testingMode :: IORef Bool
testingMode = unsafePerformIO $ newIORef False
{-# NOINLINE testingMode #-}

isBeingTested :: IO Bool
isBeingTested = readIORef testingMode


reportExceptions :: Neovim env () -> Neovim env ()
reportExceptions =
  flip catchNeovimException $ vim_report_error . mappend "UNHANDLED EXCEPTION " . show

traceMX :: Show a => String -> a -> Neovim env ()
traceMX herald a =
  vim_report_error $ "!!!" <> herald <> ": " <> show a

