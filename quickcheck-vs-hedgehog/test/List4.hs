{-# LANGUAGE DeriveFunctor #-}

module List4 (list) where

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
-- 4. Avoid counting list elements.
--
-- NOTE: (4) is a big speedup /BUT/ this will /only/ be valid if the generator
-- for the elements cannot fail.
list :: Range Int -> Gen a -> Gen [a]
list range gen = fromManual $ list' range (toManual gen)

list' :: Range Int
      -> Manual (TreeT (MaybeT Identity)  a)
      -> Manual (TreeT (MaybeT Identity) [a])
list' range gen = manualSized $ \size ->
    let minSize = Range.lowerBound size range in
    fmap (fmap sizedListElems . interleaveSizedList minSize) $ do
      n <- fmap fromJust $ dontShrink $ integral_ range
      SizedList n <$> manualReplicate n gen

{-------------------------------------------------------------------------------
  Interleaving
-------------------------------------------------------------------------------}

interleaveSizedList :: Int
                    -> SizedList (TreeT (MaybeT Identity) a)
                    -> TreeT (MaybeT Identity) (SizedList a)
interleaveSizedList minSize =
    coerce (Just . interleaveSizedList' minSize . fmap mustBeJust)
  where
    mustBeJust :: Maybe a -> a
    mustBeJust (Just a) = a
    mustBeJust Nothing  = error "interleaveSizedList: element generator failed"

-- | Interleave 'SizedList'
--
-- Precondition: number of elements in the input list must be at least
-- the minimum size.
interleaveSizedList' :: Int
                     -> SizedList (NodeT (MaybeT Identity) a)
                     -> NodeT (MaybeT Identity) (SizedList a)
interleaveSizedList' minSize = go
  where
    go :: SizedList (NodeT (MaybeT Identity) a)
       -> NodeT (MaybeT Identity) (SizedList a)
    go ts = NodeT (fmap nodeValue ts) $ concat [
          [ wrapTreeT . Just $ go ts'
          | (xs, _y, zs) <- splits (sizedListElems ts)
          , let ts' = SizedList (sizedListSize ts - 1) (xs ++ zs)
          , sizedListSize ts' >= minSize
          ]
        , [ wrapTreeT . Just $ go ts'
          | (xs, y, zs) <- splits (sizedListElems ts)
            -- /if/ we manage the shrink the element, the number of elements
            -- in the list stays the same
          , y' <- mapMaybe unwrapTreeT (nodeChildren y)
          , let ts' = SizedList (sizedListSize ts) (xs ++ [y'] ++ zs)
          ]
        ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

splits :: [a] -> [([a], a, [a])]
splits []     = []
splits (x:xs) = ([],x,xs) : fmap (\(as,b,cs) -> (x:as,b,cs)) (splits xs)

data SizedList a = SizedList {
      sizedListSize  :: !Int
    , sizedListElems :: [a]
    }
  deriving (Functor)
