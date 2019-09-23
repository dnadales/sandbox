{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Manual (
    Manual(..)
  , toManual
  , fromManual
  , dontShrink
    -- * Combinators
  , manualSized
  , manualReplicate
    -- * Auxiliary
  , wrapTreeT
  , unwrapTreeT
  ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Functor.Identity

import Hedgehog
import Hedgehog.Internal.Gen
import Hedgehog.Internal.Tree

import qualified Hedgehog.Internal.Seed as Seed

-- newtype GenT m a = GenT { unGenT :: Size -> Seed -> TreeT m a }
-- type Gen = GenT (Maybe Identity)

newtype Manual a = Manual { unManual :: Size -> Seed -> a }

toManual :: Gen a -> Manual (TreeT (MaybeT Identity) a)
toManual (GenT f) = Manual f

fromManual :: Manual (TreeT (MaybeT Identity) a) -> Gen a
fromManual (Manual f) = GenT f

dontShrink :: Gen a -> Manual (Maybe a)
dontShrink = fmap (fmap nodeValue . coerce) . toManual

instance Functor Manual where
  fmap = liftM

instance Applicative Manual where
  pure x = Manual $ \_ _ -> x
  (<*>)  = ap

instance Monad Manual where
  return         = pure
  Manual x >>= f = Manual $ \size seed ->
      case Seed.split seed of
        (sx, sf) -> unManual (f (x size sx)) size sf

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

manualSized :: (Size -> Manual a) -> Manual a
manualSized f = Manual $ \size seed -> unManual (f size) size seed

manualReplicate :: forall a. Int -> Manual a -> Manual [a]
manualReplicate n (Manual f) = Manual $ \size seed ->
    let go :: Int -> Seed -> [a]
        go 0   _ = []
        go !n' s = case Seed.split s of
                     (s', s'') -> f size s' : go (n' - 1) s''
    in go n seed


{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

wrapTreeT :: Maybe (NodeT (MaybeT Identity) a) -> TreeT (MaybeT Identity) a
wrapTreeT = coerce

unwrapTreeT :: TreeT (MaybeT Identity) a -> Maybe (NodeT (MaybeT Identity) a)
unwrapTreeT = coerce

splits :: [a] -> [([a], a, [a])]
splits []     = []
splits (x:xs) = ([],x,xs) : fmap (\(as,b,cs) -> (x:as,b,cs)) (splits xs)

-- | @removes n@ splits a list into chunks of size n and returns all possible
-- lists where one of these chunks has been removed.
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
removes k = \xs -> go xs
  where
    go :: [a] -> [[a]]
    go [] = []
    go xs = xs2 : map (xs1 ++) (go xs2)
      where
        (xs1, xs2) = splitAt k xs
