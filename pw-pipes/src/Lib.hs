module Lib
    ( someFunc
    , countLinesPp
    , printLineCount
    , countFileLines
    ) where

import           Control.Monad
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Pipes         (Pipe, Producer, await, yield, (>->))
import qualified Pipes         as Pipes
import qualified Pipes.Prelude as PipesPrelude
import           Pipes.Safe    (MonadSafe, runSafeT)
import qualified Pipes.Text.IO as PpT

someFunc :: IO ()
someFunc = putStrLn "someFunc"

countLinesPp :: Monad m => Pipe Text Int m ()
countLinesPp = forever $
  await >>= yield . length . T.lines

countFileLines :: MonadSafe m => FilePath -> Producer Int m ()
countFileLines fPath = PpT.readFile fPath >-> countLinesPp

-- | Print the number of lines in a file.
printLineCount :: FilePath -> IO ()
printLineCount fPath = do
  sum <- runSafeT $ PipesPrelude.sum (countFileLines fPath)
  print $ "Number of lines in " ++ fPath ++ ": " ++ (show sum)
