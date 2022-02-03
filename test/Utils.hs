{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Control.Concurrent (threadDelay)
import           Cornelis.Types
import           Cornelis.Types.Agda
import           Data.Bifunctor (bimap)
import           Data.Foldable (minimumBy)
import           Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Lib
import           Neovim
import           Neovim.API.Text
import           Neovim.Test
import           Plugin
import           Test.Hspec hiding (after, before)


data Diff a
  = Insert a
  | Delete a
  | Modify a a
  deriving (Eq, Ord, Show, Functor)


diff :: Eq a => [a] -> [a] -> [Diff a]
diff = (snd .) . go
  where
    go [] as = (length as, fmap Insert as)
    go bs [] = (length bs, fmap Delete bs)
    go (b : bs) (a : as)
      | b == a = go bs as
      | otherwise = do
          let x = bimap (+1) (Delete b :) $ go bs (a : as)
              y = bimap (+1) (Insert a :) $ go (b : bs) as
              z = bimap (+1) (Modify b a :) $ go bs as
          minimumBy (comparing fst) [z, x, y]


differing :: Buffer -> Neovim CornelisEnv () -> Neovim CornelisEnv [Diff Text]
differing b m = do
  before <- fmap V.toList $ buffer_get_lines b 0 (-1) False
  m
  liftIO $ threadDelay 1e5
  after <- fmap V.toList $ buffer_get_lines b 0 (-1) False
  pure $ diff before after


intervention :: Buffer -> [Diff Text] -> Neovim CornelisEnv () -> Neovim CornelisEnv ()
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


diffSpec :: String -> Seconds -> FilePath -> [Diff Text] -> (Window -> Buffer -> Neovim CornelisEnv ()) -> Spec
diffSpec name secs fp diffs m = do
  let withNeovimEmbedded f a = testWithEmbeddedNeovim f secs () a
  it name . withNeovimEmbedded Nothing $ do
    env <- cornelisInit
    withLocalEnv env $ do
      vim_command $ "edit " <> T.pack fp
      load
      liftIO $ threadDelay 1e6
      w <- vim_get_current_window
      b <- nvim_win_get_buf w
      intervention b diffs $ m w b


mkPos :: Int32 -> Int32 -> Position' LineOffset ()
mkPos line col = Pn () (Offset 0) (LineNumber $ line) $ Offset col

goto :: Window -> Buffer -> Int32 -> Int32 -> Neovim env ()
goto  w b row col = do
  (row', col') <- fmap positionToVim $ vimifyPositionM b $ mkPos row col
  -- TODO(sandy): I can't keep track of these one indicies for my life
  nvim_win_set_cursor w (row' + 1, col')

