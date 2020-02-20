
module Main where

import           Cardano.Prelude (buildAndRenderClosureTree
                   , ClosureTreeOptions (ClosureTreeOptions), ctoMaxDepth
                   , ctoCyclicClosures
                   , TraverseCyclicClosures (TraverseCyclicClosures)
                   , TreeDepth (AnyDepth))

main :: IO ()
main = print "" >> buildAndRenderClosureTree opts "bar" >>= print
  where
    opts = ClosureTreeOptions
           { ctoMaxDepth = AnyDepth
           , ctoCyclicClosures = TraverseCyclicClosures
           }
