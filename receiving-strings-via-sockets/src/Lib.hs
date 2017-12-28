{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Control.Monad
import Control.Exception.Base
import Control.Exception
import Control.Concurrent

import Network.TextViaSockets

readThreeTimes :: IO ()
readThreeTimes = readLines `catch` handler
    where readLines = do
              conn <- connectTo "localhost" "9090"
              line <- readLineFrom conn
              print line
              line <- readLineFrom conn
              print line
              line <- readLineFrom conn
              print line
              line <- readLineFrom conn
              print line
              close conn
              print "Bye bye!"
          handler :: BlockedIndefinitelyOnSTM -> IO ()              
          handler ex = putStrLn $ "I'm swallowing this: " ++ show ex

readAndCancel :: IO ()
readAndCancel = forever $ do
    tid <- forkIO readThreeTimes
    threadDelay (1 * 10^6)
    putStrLn "Killing the reader thread..."
    killThread tid
    putStrLn "Reader thread killed"
