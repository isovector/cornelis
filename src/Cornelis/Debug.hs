module Cornelis.Debug where

import Neovim
import Neovim.API.String (vim_report_error)


reportExceptions :: Neovim env () -> Neovim env ()
reportExceptions =
  flip catchNeovimException $ vim_report_error . mappend "UNHANDLED EXCEPTION " . show

traceMX :: Show a => String -> a -> Neovim env ()
traceMX herald a =
  vim_report_error $ "!!!" <> herald <> ": " <> show a

