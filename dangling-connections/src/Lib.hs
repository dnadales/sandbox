{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where


import           Control.Concurrent
import           Control.Monad
import           Data.String.Utils
import           Network
import           System.IO

someFunc :: IO ()
someFunc = withSocketsDo $ do
    h <- connectTo "localhost" (PortNumber 9090)
    ch <- newChan
    cmdsHandler h ch
    readerTid <- forkIO $ handleReader h ch
    cmdsHandler h ch
    putStrLn "Killing the handler reader"
    killThread readerTid
    putStrLn "Closing the handle"
    hClose h

cmdsHandler :: Handle -> Chan Action -> IO ()
cmdsHandler h ch = do
    act <- readChan ch
    case act of
      Quit      -> putStrLn "Bye bye"
      Line line -> do
            hPutStrLn h (reverse line)
            cmdsHandler h ch

handleReader :: Handle -> Chan Action -> IO ()
handleReader h ch = forever $ do
    line <- strip <$> hGetLine h
    case line of
        "quit" -> writeChan ch Quit
        _      -> writeChan ch (Line line)

data Action = Quit | Line String
