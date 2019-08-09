{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}

module List3 (
    list
  , interleaveList
  , interleaveListFast
  ) where

import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Functor.Identity
import Data.Maybe

import Hedgehog
import Hedgehog.Internal.Gen hiding (list, map)
import Hedgehog.Internal.Tree hiding (catMaybes)

import qualified Hedgehog.Range as Range

import Manual

-- | Generate list of elements
--
-- 1. Specializes the type of 'list'
-- 2. Takes advantage of more monomorphic type to define faster interleaving
--    (relevant even when not shrinking: first element is also produced faster)
-- 3. Use specialized version of 'replicateM'
list :: Range Int -> Gen a -> Gen [a]
list range gen = sized $ \size ->
    let minSize = Range.lowerBound size range in
    ensure (atLeast minSize) $
      fromManual $ list' range (toManual gen)

list' :: Range Int
      -> Manual (TreeT (MaybeT Identity)  a)
      -> Manual (TreeT (MaybeT Identity) [a])
list' range gen =
    fmap interleaveList $ do
      n <- fmap fromJust $ dontShrink $ integral_ range
      manualReplicate n gen

{-------------------------------------------------------------------------------
  Interleaving
-------------------------------------------------------------------------------}

interleaveList :: [TreeT (MaybeT Identity) a] -> TreeT (MaybeT Identity) [a]
interleaveList = coerce (Just . interleaveList' . catMaybes)

interleaveList' :: [NodeT (MaybeT Identity) a] -> NodeT (MaybeT Identity) [a]
interleaveList' ts =
    NodeT (map nodeValue ts) $
      concat [
          [ wrapTreeT . Just $ interleaveList' (xs ++ zs)
          | (xs, _y, zs) <- splits ts
          ]
        , [ wrapTreeT . Just $ interleaveList' (xs ++ [y'] ++ zs)
          | (xs, y, zs) <- splits ts
          , y' <- mapMaybe unwrapTreeT (nodeChildren y)
          ]
        ]

{-------------------------------------------------------------------------------
  Faster interleaving
-------------------------------------------------------------------------------}

interleaveListFast :: [TreeT (MaybeT Identity) a] -> TreeT (MaybeT Identity) [a]
interleaveListFast = coerce (Just . interleaveListFast' . catMaybes)

interleaveListFast' :: [NodeT (MaybeT Identity) a] -> NodeT (MaybeT Identity) [a]
interleaveListFast' ts =
    NodeT (map nodeValue ts) $
      concat [
          [ wrapTreeT . Just $ interleaveListFast' ts'
          | chunkSize <- chunkSizes
          , ts' <- removes chunkSize ts
          ]
        , [ wrapTreeT . Just $ interleaveListFast' (xs ++ [y'] ++ zs)
          | (xs, y, zs) <- splits ts
          , y' <- mapMaybe unwrapTreeT (nodeChildren y)
          ]
        ]
  where
    -- Chunks we try to remove from the list
    --
    -- For example, if the list has length 10, @chunkSizes = [10,5,2,1]@
    chunkSizes :: [Int]
    chunkSizes = takeWhile (>0) $ iterate (`div` 2) (length ts)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

splits :: [a] -> [([a], a, [a])]
splits []     = []
splits (x:xs) = ([],x,xs) : fmap (\(as,b,cs) -> (x:as,b,cs)) (splits xs)

-- | @removes n@ computes all ways we can remove chunks of size @n@ from a list
--
-- Examples
--
-- > removes 1 [1..3] == [[2,3],[1,3],[1,2]]
-- > removes 2 [1..4] == [[3,4],[1,2]]
-- > removes 2 [1..5] == [[3,4,5],[1,2,5],[1,2,3,4]]
-- > removes 3 [1..5] == [[4,5],[1,2,3]]
--
-- Note that the last chunk we delete might have fewer elements than @n@.
removes :: forall a. Int -> [a] -> [[a]]
removes k = \xs -> go (length xs) xs
  where
    go :: Int -> [a] -> [[a]]
    go _ [] = []
    go n xs = xs2 : map (xs1 ++) (go (n-k) xs2)
      where
        (xs1, xs2) = splitAt k xs
