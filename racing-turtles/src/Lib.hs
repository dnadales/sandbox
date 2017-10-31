{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Text                (unpack)
import           System.Info
import           System.IO
import qualified System.Process           as Process
import           Turtle

someFunc :: IO ()
someFunc = do
  hSetBuffering System.IO.stdout NoBuffering
  --                          -- Windows            | Linux      | Windows (process >= 1.6.2.0)
  -- internalExample          -- Terminates         | Terminates | Terminates
  -- procExample              -- Does not terminate | Terminates | Does not terminate
  -- sysProcExample           -- Does not terminate |            | Does not terminate
  -- procProcExample          -- Does not terminate |
  experiment2                 -- Does not terminate |            | Does not terminate
  -- forkAsyncExample         -- Does not terminate | Terminates
  -- inProcExample            -- Terminates         | Terminates
  -- sysProcExceptionExample  -- Terminates         | Terminates
  --forkAsyncExampleNoTurtle  -- Does not terminate | Terminates

mkExample :: IO () -> IO ()
mkExample otherProc =  otherProc `race` sayBye >>= print

internalExample :: IO ()
internalExample = mkExample haskellProc
  where haskellProc = forever $ putStrLn "Still alive (internal)" >> sleep 1.0

procExample :: IO ()
procExample = mkExample highlander

highlander :: IO ()
highlander = void (proc "java" ["-cp", "src", "SayHi"] empty)

sysProcExample :: IO ()
sysProcExample = mkExample sysProcHighlander
  where sysProcHighlander = Process.system "java -cp src SayHi" >>= print

experiment2 :: IO ()
experiment2 = mkExample sysProcTurtleHighlander
  where sysProcTurtleHighlander =
          void (Turtle.system theProc empty)
        theProc = Process.proc "java" ["-cp", "src", "SayHi"]

procProcExample :: IO ()
procProcExample = mkExample procProcHighlander
  where procProcHighlander = do
          let crProc = Process.proc "java" ["-cp", "src", "SayHi"]
          (_, _, _, ph) <- Process.createProcess crProc
          ec <- Process.waitForProcess ph
          print ec



inProcExample :: IO ()
inProcExample = mkExample ip
  where ip = view $ inproc "java" ["-cp", "src", "SayHi"] empty


forkAsyncExample :: IO ()
forkAsyncExample = mkExample forkAsyncHighlander
  where forkAsyncHighlander = sh $ using $ do
          a <- fork highlander
          Turtle.wait a

forkAsyncExampleNoTurtle :: IO ()
forkAsyncExampleNoTurtle = mkExample forkAsyncHighlander
  where forkAsyncHighlander = withAsync highlander Control.Concurrent.Async.wait

sysProcExceptionExample :: IO ()
sysProcExceptionExample = mkExample aMereMortalProc
  where aMereMortalProc = do
          mJavaPath <-  which (fromText javaCmd)
          case toText <$> mJavaPath of
            Nothing -> putStrLn "I cannot find the java executable"
            Just (Left _) -> putStrLn "I cannot decode the java executable path"
            Just (Right jPath) -> do
              ph <- Process.spawnProcess (unpack jPath) ["-cp", "src", "SayHi"]
              waitForProcess `onException` Process.terminateProcess ph
            where waitForProcess =
                    forever (sleep 60.0) >> putStrLn "Waiting for process"

addExeSuffix :: Text -> Text
addExeSuffix path = if os == "mingw32" then path <> ".exe" else path

javaCmd :: Text
javaCmd = addExeSuffix "java"

sayBye :: IO ()
sayBye = sleep 5.0 >> putStrLn "Bye!"

