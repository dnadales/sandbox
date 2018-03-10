module Main where

import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
    
import qualified NumberStreamServer
import qualified EchoServer

-- | Testing the echo server:
--
-- > curl -XPOST http://localhost:8081/session/new
-- > curl -XPOST http://localhost:8081/session/0/echo -H "Content-Type: application/json" -d '{"msgText": "World"}'
--
-- To subscribe to the events use:
-- 
-- > curl http://localhost:8081/session/0/events
--

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["stream"] ->
            run 8081 NumberStreamServer.app
        ["sse"]    ->  do
            env <- EchoServer.newEnv
            run 8081 (EchoServer.app env)
        _         ->
            putStrLn "Usage: streaming-endpoints-exe (stream | sse)"
