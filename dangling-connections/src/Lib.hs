{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where


import           Network
import           System.IO

someFunc :: IO ()
someFunc = withSocketsDo $ do
    sock <- listenOn (PortNumber 9090)
    (h, _, _) <- accept sock
    handleCmds h

handleCmds :: Handle -> IO ()
handleCmds h = do
    line <- hGetLine h
    case line of
        "quit" -> do
            putStrLn "Closing the handle"
            hClose h
            putStrLn "Bye bye"
        _ -> do
            putStrLn $ "echo \"" ++ line ++ "\""
            handleCmds h
