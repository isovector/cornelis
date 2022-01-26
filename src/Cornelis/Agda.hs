module Cornelis.Agda where
import System.IO
import Control.Monad.IO.Class
import System.Process

data Agda = Agda
  { a_req  :: Handle
  , a_resp :: Handle
  }

spawnAgda :: MonadIO m => m Agda
spawnAgda = do
  (min, mout, _, _) <- liftIO $ createProcess $ proc "agda" ["--interaction-json"]
  case (min, mout) of
    (Just hin, Just hout) -> pure $ Agda hin hout
    (_, _) -> error "can't start agda"

