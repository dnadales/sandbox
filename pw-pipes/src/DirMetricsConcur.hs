-- | Directory metrics, using Pipes and Pipes.Concurrent.

module DirMetricsConcur (printLinesCountIn) where

import           Control.Concurrent.Async
import qualified DirMetricsPipes          as DM
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude            as Pipes
import           Pipes.Safe

printLinesCountIn :: Int -> FilePath -> IO ()
printLinesCountIn nThreads fPath = do
  (outFPath, inFPath, sealFP) <- spawn' unbounded
  (outFCount, inFCount, sealFC) <- spawn' unbounded
  forkIO $ do
    runSafeT $ runEffect $
      every (DM.regularFilesIn fPath) >-> toOutput outFPath
    atomically $ sealFP
  forkIO $ do
    replicateConcurrently_ nThreads $ do
      runSafeT $ runEffect $
        for (fromInput inFPath) DM.fileContents
        >-> DM.byteStringAsText
        >-> DM.countLines
        >-> toOutput outFCount
    atomically $ sealFC
  n <- Pipes.sum (fromInput inFCount)
  putStrLn (show n)
