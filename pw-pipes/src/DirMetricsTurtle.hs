-- | Directory metrics, using Turtle.

module DirMetricsTurtle
  ( printFilesCountIn
  , printLinesCountIn
  ) where

import qualified Control.Foldl as F
import qualified Turtle        as T

-- | Print the number of regular files in a directory. This should implement:
--
-- > find $fPath -type f | wc -l
printFilesCountIn :: T.FilePath -> IO ()
printFilesCountIn fPath = do
  count <- T.fold ( do
                      fInFPath <- T.lsif noSymLink fPath
                      st <- T.stat fInFPath
                      if T.isRegularFile st
                        then return 1
                        else return 0
                  ) (F.sum)
  print count
  where noSymLink f = do
          st <- T.stat f
          return (not (T.isSymbolicLink st))


-- | Print the number of lines in all the files in a directory.
printLinesCountIn :: T.FilePath -> IO ()
printLinesCountIn = undefined

