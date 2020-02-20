{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Text as T
import           Text.Pretty.Simple (pPrint)
import           Cardano.Prelude (buildAndRenderClosureTree, force, ($!!), deepseq
                   , ClosureTreeOptions (ClosureTreeOptions), ctoMaxDepth
                   , ctoCyclicClosures, buildClosureTree
                   , TraverseCyclicClosures (TraverseCyclicClosures)
                   , TreeDepth (AnyDepth), renderTree)

main :: IO ()
main = do
  Just t <- buildClosureTree opts $!! bar
  pPrint t
  pPrint $ renderTree t (T.pack . show)
  where
    bar = "bar"
    opts = ClosureTreeOptions
           { ctoMaxDepth = AnyDepth
           , ctoCyclicClosures = TraverseCyclicClosures
           }
