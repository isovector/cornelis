{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Utils
  ( module Utils
  , Edit (..)
  ) where

import           Control.Concurrent (threadDelay)
import           Cornelis.Types
import           Cornelis.Offsets
import           Cornelis.Utils (withLocalEnv)
import           Cornelis.Vim
import           Data.Foldable.Levenshtein (levenshtein, Edit(..))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Lib
import           Neovim
import           Neovim.API.Text
import           Neovim.Test
import           Plugin
import           System.FilePath (takeBaseName)
import           System.IO (hFlush, hPutStr)
import           System.IO.Temp (withSystemTempFile)
import           Test.Hspec hiding (after, before)


isCopy :: Edit a -> Bool
isCopy Copy{} = True
isCopy _ = False

diff :: Show a => Eq a => [a] -> [a] -> [Edit a]
diff = ((filter (not . isCopy) . snd) .) . levenshtein @_ @_ @_ @Int


differing :: Buffer -> Neovim env () -> Neovim env [Edit Text]
differing b m = do
  before <- fmap V.toList $ buffer_get_lines b 0 (-1) False
  m
  liftIO $ threadDelay 1e6
  after <- fmap V.toList $ buffer_get_lines b 0 (-1) False
  pure $ diff before after


intervention :: Buffer -> [Edit Text] -> Neovim env () -> Neovim env ()
intervention b d m = do
  d' <- differing b m
  liftIO $ d' `shouldBe` d

withVim :: Seconds -> (Window -> Buffer -> Neovim () ()) -> IO ()
withVim secs m = do
  let withNeovimEmbedded f a = testWithEmbeddedNeovim f secs () a
  withNeovimEmbedded Nothing $ do
    b <- nvim_create_buf False False
    w <- vim_get_current_window
    nvim_win_set_buf w b
    m w b


diffSpec
    :: String
    -> Seconds
    -> FilePath
    -> [Edit Text]
    -> (Window -> Buffer -> Neovim CornelisEnv ())
    -> Spec
diffSpec name secs fp diffs m =
  vimSpec name secs fp $ \w b -> intervention b diffs $ m w b


vimSpec
    :: String
    -> Seconds
    -> FilePath
    -> (Window -> Buffer -> Neovim CornelisEnv ())
    -> Spec
vimSpec name secs fp m = do
  let withNeovimEmbedded f a = testWithEmbeddedNeovim f secs () a
  it name $ do
    withSystemTempFile "test.agda" $ \fp' h -> do
      hPutStr h $ "module " <> takeBaseName fp' <> " where\n"
      hPutStr h =<< fmap (unlines . tail . lines) (readFile fp)
      hFlush h
      withNeovimEmbedded Nothing $ do
        env <- cornelisInit
        withLocalEnv env $ do
          vim_command $ "edit " <> T.pack fp'
          liftIO $ threadDelay 1e6
          w <- vim_get_current_window
          b <- nvim_win_get_buf w
          load
          liftIO $ threadDelay 5e6
          m w b

goto :: Window -> Int -> Int -> Neovim env ()
goto w row col = setWindowCursor w $ Pos (toOneIndexed row) (toOneIndexed col)

pressKeys :: Window -> Text -> Neovim env ()
pressKeys w t = do
  vim_set_current_window w
  vim_command $ "norm! " <> t


