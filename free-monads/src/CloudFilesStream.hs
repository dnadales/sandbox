{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}
module CloudFiles where
-- | See http://stackoverflow.com/questions/40105759/logging-using-the-free-monad/40110592?noredirect=1#comment67529569_40110592

import           Control.Monad.Free
import qualified Control.Monad.Trans.Free as FT
import           Data.Functor.Compose
import           Data.Functor.Sum
import           Data.List.Split
import           Prelude                  hiding (log)
import           Streaming
                  -- this is the same as FreeT but with better combinators
                  -- e.g. "zips" and "unzips" "separate" "unseparate"


type Path = String
type Bytes = String

--------------------------------------------------------------------------------
-- The DSL for cloud files.
--------------------------------------------------------------------------------

data CloudFilesF a
  = SaveFile Path Bytes a
  | ListFiles Path ([Path] -> a)
  deriving (Functor)
--------------------------------------------------------------------------------

saveFile :: Path -> Bytes -> Free CloudFilesF ()
saveFile path bytes = liftF $ SaveFile path bytes ()

listFiles :: Path -> Free CloudFilesF [Path]
listFiles path = liftF $ ListFiles path id

--------------------------------------------------------------------------------
-- The DSL for logging.
--------------------------------------------------------------------------------

data Level = Debug | Info | Warning | Error deriving Show
data LogF a = Log Level String a deriving Functor
-------------------------------------------------------------

log :: Level -> String -> Free LogF ()
log level msg = liftF $ Log level msg ()

--------------------------------------------------------------------------------
-- The DSL for REST clients.
--------------------------------------------------------------------------------

data RestF a = Get Path (Bytes -> a)
             | Put Path Bytes (Bytes -> a) -- | TODO: why does get returns Bytes?
             deriving Functor
--------------------------------------------------------------------------------

get :: Path -> Free RestF Bytes
get path = liftF $ Get path id

put :: Path -> Bytes -> Free RestF Bytes
put path bytes = liftF $ Put path bytes id


---------------------------------
-- sample programs
---------------------------------

-- Test the interpreter for the REST DSL with a program.
sampleProgram :: Free RestF Bytes
sampleProgram = do
  put "/artist/0" "juan"
  get "/artist/0"

-- Test the intepreter for the cloud DSL that used the REST DSL.
sampleCloudFilesProgram :: Free CloudFilesF ()
sampleCloudFilesProgram = do
  saveFile "/myfolder/pepino" "verde"
  saveFile "/myfolder/tomate" "rojo"
  _ <- listFiles "/myfolder"
  return ()

-------------------------------
-- utility to convert Free to FreeT/Stream
-- in fact we just use this for the manipulation with "zips"
-------------------------------
toStream  :: (Functor f, Monad m) => Free f a -> Stream f m a
toStream = foldFree yields


-- | An interpreter for the cloud DSL that uses the REST DLS.
interpretCloudWithRest :: CloudFilesF a -> Free RestF a
interpretCloudWithRest (SaveFile path bytes next) = do
  put path bytes
  return next
interpretCloudWithRest (ListFiles path withFiles) = do
  content <- get path
  let files = splitOn "/" content
  return (withFiles files)


interpretCloudWithLogging :: CloudFilesF r -> Compose LogF CloudFilesF r
interpretCloudWithLogging str = Compose $ case str of
  a@(SaveFile p _ _) -> Log Debug ("Saving file to " ++ show p) a
  a@(ListFiles p _)  -> Log Debug ("Listing files at " ++ show p) a


interpretRestIO :: MonadIO m => RestF r -> m r
interpretRestIO (Get path withResponse) = do
  liftIO $ putStrLn $ "I should GET " ++ path
  return $ withResponse "mocked PUT response"
interpretRestIO (Put path bytes withResponse) = do
  liftIO $ putStrLn $ "I should PUT " ++ path ++ " " ++ bytes
  return $ withResponse "mocked PUT response"

interpretLogIO :: MonadIO m => LogF r -> m r
interpretLogIO (Log a b c) =  liftIO $ do
  putStr "Level: "
  print a
  putStr "Message: "
  print b
  return c

addLogging ::  Monad m => Stream CloudFilesF m r -> Stream LogF (Stream CloudFilesF m) r
addLogging = unzips . maps interpretCloudWithLogging

runLogger :: MonadIO m => Stream LogF m r -> m r
runLogger  =  run . maps interpretLogIO

cloudFilesToRestF :: Monad m => Stream CloudFilesF m r -> Stream RestF m r
cloudFilesToRestF = concats . maps (toStream . interpretCloudWithRest)

runRest :: Stream RestF IO r -> IO r
runRest = run . maps interpretRestIO

------------------------
-- composed interpreters
-------------------------

test4 :: Free CloudFilesF r -> IO r
test4 = runRest  . cloudFilesToRestF . runLogger . addLogging  . toStream

-- the same, running the internal monad 'first'
test3 :: Free CloudFilesF r -> IO r
test3 = runLogger . hoist (runRest . cloudFilesToRestF) . addLogging . toStream

test2 :: Free CloudFilesF r -> IO r
test2 = runRest  . cloudFilesToRestF . toStream

test1 :: Free RestF r -> IO r
test1 = runRest . toStream


main_ = test2 sampleCloudFilesProgram
main__ = test1 sampleProgram
