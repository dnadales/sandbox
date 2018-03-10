module Main where

import Network.Wai.Handler.Warp

import qualified Lib
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
    env <- EchoServer.newEnv
    run 8081 $ EchoServer.app env
