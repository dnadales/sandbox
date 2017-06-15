-- | Directory metrics, using Turtle.

module DirMetricsTurtle
  ( printFilesCountIn
  , printLinesCountIn
  , regularFilesIn
  ) where

import qualified Control.Foldl as F
import qualified Turtle        as T

-- | Returns true iff the file path is not a symlink.
noSymLink :: T.FilePath -> IO Bool
noSymLink fPath = (not . T.isSymbolicLink) <$> T.stat fPath

-- | Shell that outputs the regular files in the given directory.
regularFilesIn :: T.FilePath -> T.Shell T.FilePath
regularFilesIn fPath = do
  fInFPath <- T.lsif noSymLink fPath
  st <- T.stat fInFPath
  if T.isRegularFile st
    then return fInFPath
    else T.empty

-- | Print the number of regular files in a directory. This should implement:
--
-- > find $fPath -type f | wc -l
printFilesCountIn :: T.FilePath -> IO ()
printFilesCountIn fPath = do
  count <- T.fold (regularFilesIn fPath) F.length
  print count

-- | Print the number of lines in all the files in a directory.
printLinesCountIn :: T.FilePath -> IO ()
printLinesCountIn = undefined

