-- | Directory metrics, using Turtle.

module DirMetricsTurtle
  ( printFilesCountIn
  , printLinesCountIn
  , regularFilesIn
  , printLinesCountIn'
  , catDir
  ) where

import qualified Control.Foldl             as F
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.List.NonEmpty        as NE
import qualified Data.Text                 as Tx
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import           Filesystem.Path.CurrentOS (decodeString)
import qualified Turtle                    as T
import qualified Turtle.Bytes              as TB

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

-- | Read lines of `Text` from all the regular files under the given directory
-- path.
inputDir :: T.FilePath -> T.Shell T.Line
inputDir fPath = do
  file <- regularFilesIn fPath
  T.input file

-- | Print the number of lines in all the files in a directory.
printLinesCountIn :: T.FilePath -> IO ()
printLinesCountIn fPath = do
  count <- T.fold (inputDir fPath) F.length
  print count

-- * Trying to solve the problem with non-utf8 encoding.

mDecodeByteString :: T.Shell ByteString -> T.Shell T.Text
mDecodeByteString = gMDecodeByteString (streamDecodeUtf8With lenientDecode)
  where gMDecodeByteString :: (ByteString -> Decoding)
                             -> T.Shell ByteString
                             -> T.Shell T.Text
        gMDecodeByteString f bss = do
          bs <- bss
          let Some res bs' g = f bs
          if BS.null bs'
            then return res
            else gMDecodeByteString g bss

inputDir' :: T.FilePath -> T.Shell T.Line
inputDir' fPath = do
  file <- regularFilesIn fPath
  text <- mDecodeByteString (TB.input file)
  T.select (NE.toList $ T.textToLines text)

-- | Print the number of lines in all the files in a directory. Using a more
-- robust version of `inputDir`.
printLinesCountIn' :: FilePath -> IO ()
printLinesCountIn' fPath = do
  count <- T.fold (inputDir' fPath') T.countLines
  print count
  where fPath' = decodeString fPath

-- | Cat the directory contents.
catDir :: T.FilePath -> IO ()
catDir fPath = T.stdout (inputDir' fPath)
