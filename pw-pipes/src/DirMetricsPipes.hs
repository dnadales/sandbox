{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | Directory metrics, using Pipes.

module DirMetricsPipes
  ( printLinesCountIn
  , catDir
  , printFilesCountIn
  , regularFilesIn
  , byteStringAsText
  , countLines
  , fileContents
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.DirStream
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import qualified Filesystem.Path           as FP
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           Pipes
import qualified Pipes.ByteString          as PBS
import qualified Pipes.Prelude             as Pipes
import           Pipes.Prelude.Text
import           Pipes.Safe
import           System.Directory
import           System.IO

-- | Cat the directory contents.
catDir :: FilePath -> IO ()
catDir fPath = runSafeT $ runEffect $
  dirContents fPath >-> byteStringAsText >-> stdoutLn

-- | Print the number of regular files in a directory. This should implement:
--
-- > find $fPath -type f | wc -l
printFilesCountIn :: FilePath -> IO ()
printFilesCountIn fPath = do
  n <- runSafeT $ Pipes.length (every (regularFilesIn fPath))
  putStrLn (show n)

-- | Print the number of lines in all the files in a directory.
printLinesCountIn :: FilePath -> IO ()
printLinesCountIn fPath = do
  n <- runSafeT $ Pipes.sum $
       dirContents fPath
       >-> byteStringAsText
       >-> countLines
  putStrLn (show n)

-- | List the regular files in a directory.
regularFilesIn :: MonadSafe m => FilePath -> ListT m FilePath
regularFilesIn path = do
  child <- encodeString <$> childOf (decodeString path)
  isDir <- liftIO $ doesDirectoryExist child
  isSymLink <- liftIO $ pathIsSymbolicLink child
  if isDir && not isSymLink
    then regularFilesIn child
    else if not isSymLink
         then return child
         else empty

-- | Count the lines that pass through this pipe.
countLines :: Monad m => Pipe Text Int m ()
countLines = forever $
    await >>= yield . T.count "\n"

-- | List the contents of a file.
fileContents :: (MonadSafe m, MonadIO m) => FilePath -> Producer' ByteString m ()
fileContents fPath =
 bracket (liftIO (tryOpen fPath)) (liftIO . tryClose) tryRead
 where tryOpen fPath = do
         perm <- getPermissions fPath
         if readable perm
           then Just <$> openFile fPath ReadMode
           else return $ Nothing

       tryClose Nothing  = return ()
       tryClose (Just h) = hClose h

       tryRead Nothing  = return ()
       tryRead (Just h) = PBS.fromHandle h
-- |
dirContents :: (MonadSafe m, MonadIO m) => FilePath -> Producer' ByteString m ()
dirContents fPath = for (every (regularFilesIn fPath)) fileContents

byteStringAsText :: Monad m => Pipe ByteString Text m ()
byteStringAsText  = loop (streamDecodeUtf8With lenientDecode)
  where loop f = do
          bs <- await
          let Some res bs' g = f bs
          if (BS.null bs')
            then yield res >> byteStringAsText
            else loop g
