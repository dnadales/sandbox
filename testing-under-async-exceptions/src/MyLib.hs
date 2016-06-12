-- |

module MyLib where

import           Control.Concurrent

casMVar :: Eq a => MVar a -> a -> a -> IO Bool
casMVar m old new = do
  cur <- takeMVar m
  if cur == old
  then putMVar m new >> return True
  else putMVar m old >> return False
