module Cornelis.Debug where

import Control.Exception (catch, throw)
import System.IO.Error (isAlreadyInUseError)
import Neovim
import Neovim.API.String (vim_report_error)


reportExceptions :: Neovim env () -> Neovim env ()
reportExceptions =
  flip catchNeovimException $ vim_report_error . mappend "UNHANDLED EXCEPTION " . show

traceMX :: Show a => String -> a -> Neovim env ()
traceMX herald a =
  vim_report_error $ "!!!" <> herald <> ": " <> show a

debug :: Show a => a -> Neovim env ()
debug x = liftIO $ go 100
  where
    go 0 = pure ()
    go n = 
      catch
       (appendFile  "/tmp/agda.log" (show x <> "\n"))
       (\e -> if isAlreadyInUseError e then go (n-1 :: Int) else throw e)
