{-# LANGUAGE RankNTypes #-}
module Lib
    ( someFunc
    , countLinesPp
    , printLineCount
    , countFileLines
    , printLinesIn
    , countFilesIn
    , listFilesIn
    , concatFilesIn
    , catFile
    , concatFilesInStr
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString           (ByteString)
import           Data.DirStream
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding
import qualified Filesystem.Path           as FS
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           Pipes
import qualified Pipes.ByteString          as PB
import qualified Pipes.Prelude             as PP
import qualified Pipes.Prelude.Text        as PPT
import           Pipes.Safe                (MonadSafe, SafeT, bracket, liftBase,
                                            runSafeT)
import           Pipes.Safe.Prelude        (withFile)
import qualified Pipes.Text.Encoding       as PT
import           System.Directory
import qualified System.IO                 as IO

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Count the lines that pass through this pipe.
countLinesPp :: Monad m => Pipe Text Int m ()
countLinesPp = forever $
  await >>= yield . length . filter T.null . T.lines

-- | Count the lines of a file.
countFileLines :: FilePath -> Producer Int (SafeT IO) ()
countFileLines fPath =
  readFileAsText fPath >-> countLinesPp

-- | Pass only the existing files.
existingFiles :: MonadIO m => Pipe FilePath FilePath m ()
existingFiles = forever $ do
  fp <- await
  exists <- lift $ liftIO $ doesFileExist fp
  when exists (yield fp)

-- | Pass only the readable files.
readableFiles :: MonadIO m => Pipe FilePath FilePath m ()
-- Note that this pattern could be abstracted. Maybe in the dirstream library.
readableFiles = forever $ do
  fp <- await
  perms <- lift $ liftIO $ getPermissions fp
  when (readable perms) (yield fp)

-- | Pass only regular files (no symbolic links)
regularFiles  :: MonadIO m => Pipe FilePath FilePath m ()
regularFiles = forever $ do
  fp <- await
  symLink <- lift $ liftIO $ pathIsSymbolicLink fp
  when (not symLink) (yield fp)

-- | Print the number of lines in a file.
printLineCount :: FilePath -> IO ()
printLineCount fPath = do
  sum <- runSafeT $ PP.sum $
         for files countFileLines
  print $ "Number of lines in " ++ fPath ++ ": " ++ (show sum)
  where files = yield fPath >-> existingFiles >-> readableFiles

-- | Cat a file
catFile :: FilePath -> IO ()
catFile fPath = do
  sum <- runSafeT $ runEffect $
         files >-> catFiles >-> PP.map T.unpack >-> PP.stdoutLn
  print $ "Number of lines in " ++ fPath ++ ": " ++ (show sum)
  where files = yield fPath >-> existingFiles >-> readableFiles


readFileAsText :: FilePath -> Producer' Text (SafeT IO) ()
readFileAsText fPath =
  readFileBS fPath >-> decodeUtf8Pp

decodeUtf8Pp :: Monad m => Pipe ByteString Text m ()
decodeUtf8Pp = do
  chunk <- await
  yield $ decodeUtf8With ignore chunk
  where ignore _ _ = Just '?'

readFileBS :: FilePath -> Producer' ByteString (SafeT IO) ()
readFileBS file =
  bracket (IO.openFile file IO.ReadMode) (IO.hClose) PB.fromHandle

catFiles :: Pipe FilePath Text (SafeT IO) ()
catFiles = forever $ do
  fPath <- await
  for (readFileAsText fPath) yield

catFiles' :: Pipe FilePath Text (SafeT IO) ()
catFiles' = forever $ do
  fPath <- await
  bracket (IO.openFile fPath IO.ReadMode) (IO.hClose) PPT.fromHandleLn

one :: Monad m => Pipe a Int m ()
one = await >> yield 1 >> one

myDescentOf :: MonadSafe m => FS.FilePath -> ListT m FS.FilePath
myDescentOf path = do
  child <- childOf path
  isDir <- liftIO $ isDirectory child
  isSymLink <- liftIO $ pathIsSymbolicLink (encodeString child)
  if isDir && not isSymLink
    then return child <|> myDescentOf child
    else return child

-- | List the files in a directory.
listFilesIn :: FilePath -> IO ()
listFilesIn fPath = do
  IO.withFile "mystdout" IO.WriteMode $ \h ->
    runSafeT $ runEffect $
           every (myDescentOf (decodeString fPath))
       >-> PP.map encodeString
       >-> readableFiles
       >-> existingFiles
       >-> regularFiles
       >-> PP.toHandle h

-- | Count the number of files in a directory.
-- PROBLEM: this will list the symbolic files as well!
countFilesIn :: FilePath -> IO ()
countFilesIn fPath = do
  sum <- runSafeT $ PP.sum $
         every (myDescentOf (decodeString fPath))
     >-> PP.map encodeString
     >-> readableFiles
     >-> existingFiles
     >-> regularFiles
     >-> one
  print $ "Total number of lines of files under " ++ fPath ++ ": " ++ (show sum)

-- | Print the total number of lines of all the files in a directory.
--
-- This should implement:
--
-- > find $fPath -type f -print | xargs cat | wc -l
--
printLinesIn :: FilePath -> IO ()
printLinesIn fPath = do
  sum <- runSafeT $ PP.sum $
        every (myDescentOf (decodeString fPath))
    >-> PP.map encodeString
    >-> readableFiles
    >-> existingFiles
    >-> regularFiles
    >-> catFiles'
    >-> one
--    >-> countLinesPp
  print $ "Total number of lines under directory " ++ fPath ++ ": " ++ (show sum)

-- | Make a huge file by concatenating all the files in the directory.
concatFilesIn :: FilePath -> IO ()
concatFilesIn fPath = do
  IO.withFile "mylargefile" IO.WriteMode $ \h ->
    runSafeT $ runEffect $
           every (myDescentOf (decodeString fPath))
       >-> PP.map encodeString
       >-> readableFiles
       >-> existingFiles
       >-> regularFiles
       >-> catFiles'
       >-> PP.map T.unpack
       >-> PP.toHandle h

readFileAsStr :: Pipe FilePath String (SafeT IO) ()
readFileAsStr = forever $ do
  fPath <- await
  bracket (IO.openFile fPath IO.ReadMode) (IO.hClose) PP.fromHandle

-- This will fail when trying to convert to the file contents to utf 8 it seems:
--
-- > *** Exception: /usr/include/php/ext/standard/php_ext_syslog.h: hGetLine: invalid argument (invalid byte sequence)
--
-- Maybe we just need to count the number of new-lines:
--
--   - http://www.scs.stanford.edu/16wi-cs240h/slides/iteratee-slides.html#(15)
--
-- > L8.break (== '\n')
--
concatFilesInStr :: FilePath -> IO ()
concatFilesInStr fPath = do
  IO.withFile "mylargefileStr" IO.WriteMode $ \h ->
    runSafeT $ runEffect $
           every (myDescentOf (decodeString fPath))
       >-> PP.map encodeString
       >-> readableFiles
       >-> existingFiles
       >-> regularFiles
       >-> readFileAsStr
       >-> PP.toHandle h

-- TODO: look at how the Turtle library handles this:
--
-- >

-- TODO: test with /usr/include/php/ext/standard/
-- TODO: test with `/usr/include/libkern`
