module List3 (list) where

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
  Auxiliary
-------------------------------------------------------------------------------}

splits :: [a] -> [([a], a, [a])]
splits []     = []
splits (x:xs) = ([],x,xs) : fmap (\(as,b,cs) -> (x:as,b,cs)) (splits xs)
