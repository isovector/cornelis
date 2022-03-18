{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Utils
  ( module Utils
  , Edit (..)
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Lens (set, _head)
import           Cornelis.Types
import           Cornelis.Types.Agda
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
import           System.Random (randomIO)
import           Test.Hspec hiding (after, before)


-- data Diff a
--   = Insert a
--   | Delete a
--   | Modify a a
--   deriving (Eq, Ord, Show, Functor)

isCopy :: Edit a -> Bool
isCopy Copy{} = True
isCopy _ = False

diff :: Show a => Eq a => [a] -> [a] -> [Edit a]
diff = ((filter (not . isCopy) . snd) .) . levenshtein @_ @_ @_ @Int
  -- (snd .) . go
  -- where
  --   go ([]) as = (length as, fmap Insert as)
  --   go bs [] = (length bs, fmap Delete bs)
  --   go (b : bs) (a : as)
  --     | b == a = go bs as
  --     | otherwise = do
  --         let x = traceShowId $ bimap (+1) (Delete b :) $ go bs (a : as)
  --             y = traceShowId $ bimap (+1) (Insert a :) $ go (b : bs) as
  --             z = traceShowId $ bimap (+1) (Modify b a :) $ go bs as
  --         minimumBy (comparing fst) [z, x, y]


differing :: Buffer -> Neovim env () -> Neovim env [Edit Text]
differing b m = do
  before <- fmap V.toList $ buffer_get_lines b 0 (-1) False
  m
  liftIO $ threadDelay 1e5
  after <- fmap V.toList $ buffer_get_lines b 0 (-1) False
  liftIO $ writeFile "/tmp/done.agda" $ T.unpack $ T.unlines after
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


randomModuleName :: IO String
randomModuleName = do
  x <- randomIO @Int
  pure $ "test-" <> show x


vimSpec
    :: String
    -> Seconds
    -> FilePath
    -> (Window -> Buffer -> Neovim CornelisEnv ())
    -> Spec
vimSpec name secs fp m = do
  let withNeovimEmbedded f a = testWithEmbeddedNeovim f secs () a
  it name $ do
    modName <- randomModuleName
    fc <- fmap ( unlines
               . set _head ("module " <> modName <> " where")
               . lines
               ) $ readFile fp
    let fp' = "/tmp/" <> modName <> ".agda"
    writeFile fp' fc
    withNeovimEmbedded Nothing $ do
      env <- cornelisInit
      withLocalEnv env $ do
        vim_command $ "edit " <> T.pack fp'
        load
        liftIO $ threadDelay 1e6
        w <- vim_get_current_window
        b <- nvim_win_get_buf w
        m w b
        liftIO $ threadDelay 5e6


mkPos :: Int32 -> Int32 -> Pos
mkPos line col = Pos (LineNumber line) $ Offset col

goto :: Window -> Int32 -> Int32 -> Neovim env ()
goto w row col = setWindowCursor w $ mkPos row col

