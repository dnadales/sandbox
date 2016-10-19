{-# LANGUAGE DeriveFunctor #-}
-- | Work on the ideas presented at http://degoes.net/articles/modern-fp

module CloudFiles where

import           Control.Monad.Free
import           Data.Functor.Coproduct
import           Data.Functor.Sum
import           Data.List.Split
import           Prelude                hiding (log)

--------------------------------------------------------------------------------
-- The DSL for cloud files.
data CloudFilesF a
  = SaveFile Path Bytes a
  | ListFiles Path ([Path] -> a)
  deriving Functor

type Path = String
type Bytes = String

saveFile :: Path -> Bytes -> Free CloudFilesF ()
-- | To define `saveFile` we use `liftF`:
--
--       liftF :: (Functor f, MonadFree f m) => f a -> m a
--
saveFile path bytes = liftF $ SaveFile path bytes ()

listFiles :: Path -> Free CloudFilesF [Path]
listFiles path = liftF $ ListFiles path id

--------------------------------------------------------------------------------
-- The DSL for logging.
data Level = Debug | Info | Warning | Error deriving Show
data LogF a = Log Level String a deriving Functor

-- | Utility functions to build log programs.
log :: Level -> String -> Free LogF ()
log level msg = liftF $ Log level msg ()

-- | An interpreter for the logging DSL, in terms of the IO monad.
interpLogIO :: LogF a -> IO a
interpLogIO (Log level msg next) = do
  putStrLn $ (show level) ++ ": " ++ msg
  return next

--------------------------------------------------------------------------------
-- The DSL for REST clients.
data RestF a = Get Path (Bytes -> a)
             | Put Path Bytes (Bytes -> a)
             deriving Functor

get :: Path -> Free RestF Bytes
get path = liftF $ Get path id

put :: Path -> Bytes -> Free RestF Bytes
put path bytes = liftF $ Put path bytes id

-- | An interpreter for the cloud DSL that uses the REST DLS.
cloudFtoRestM :: CloudFilesF a -> Free RestF a
cloudFtoRestM (SaveFile path bytes next) = do
  put path bytes
  return next

-- | For this case let's do something slightly more interesting.
cloudFtoRestM (ListFiles path withFiles) = do
  content <- get path
  let files = splitOn " " content
  return (withFiles files)

-- | A decorator for the cloud DSL that adds logging.
--
addLogToRest :: CloudFilesF a -> Free (Sum LogF RestF) a
addLogToRest inst@(SaveFile path bytes _) = do
  hoistFree InL $ log Debug ("Saving " ++ bytes ++ " to " ++ path)
  next <- hoistFree InR $ cloudFtoRestM inst
  hoistFree InL $ log Debug ("Saved to " ++ path)
  return next

addLogToRest inst@(ListFiles path _) = do
  hoistFree InL $ log Debug ("Listing contents in " ++ path)
  next <- hoistFree InR $ cloudFtoRestM inst
  hoistFree InL $ log Debug ("Listing done in " ++ path)
  return next

interpRestIO :: RestF a -> IO a
interpRestIO (Get path withResponse) = do
  putStrLn $ "I should GET " ++ path
  result <- return "mocked GET response"
  return (withResponse result)

interpRestIO (Put path bytes withResponse) = do
  putStrLn $ "I should PUT " ++ path ++ " " ++ bytes
  result <- return "mocked PUT response"
  return (withResponse result)

interpLogRestIO :: (Sum LogF RestF) a -> IO a
interpLogRestIO (InL logF) = interpLogIO logF
interpLogRestIO (InR restF) = interpRestIO restF

-- Test the interpreter for the REST DSL with a program.
sampleProgram :: Free RestF Bytes
sampleProgram = do
  put "/artist/0" "juan"
  get "/artist/0"

runSampleProgram = foldFree interpRestIO sampleProgram

-- Test the intepreter for the cloud DSL that used the REST DSL.
sampleCloudFilesProgram :: Free CloudFilesF ()
sampleCloudFilesProgram = do
  saveFile "/myfolder/pepino" "verde"
  saveFile "/myfolder/tomate" "rojo"
  _ <- listFiles "/myfolder"
  return ()

-- | Run the sample program using the REST interpreter.
runSampleCloudProgram =
  foldFree interpRestIO $ foldFree cloudFtoRestM sampleCloudFilesProgram

interpretClouldFilesProgram :: Free CloudFilesF a -> IO a
interpretClouldFilesProgram = foldFree interpLogRestIO . foldFree addLogToRest

runSampleCloudProgram1 = interpretClouldFilesProgram sampleCloudFilesProgram

-- | The upshot after doing these exercises seems to be that the interpreters
-- always have type:
--
--     f a -> m a
--
-- where `f` is a functor and `m` is a monad. In case we are interpreting
-- functors in terms of other functors, `m` will be the free monad.
--
--     f a -> Free g a
--
-- for some other functor `g`.
--
-- At the borders of the application, we will be using mostly an iterpreter
-- with type:
--
--    f a -> IO a
--


