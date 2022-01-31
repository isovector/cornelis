module Cornelis.Debug where
import Neovim
import Neovim.API.String (vim_report_error)

reportExceptions :: Neovim env () -> Neovim env ()
reportExceptions = flip catchNeovimException $ \exc -> vim_report_error $ "UNHANDLED EXCEPTION" <> show exc
