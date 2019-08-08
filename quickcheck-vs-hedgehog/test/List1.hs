module List1 (list) where

import Hedgehog

import qualified Hedgehog.Gen as Gen

-- | Generate list of elements
--
-- Specializes the type of 'list'. Original:
--
-- > list :: MonadGen m => Range Int -> m a -> m [a]
list :: Range Int -> Gen a -> Gen [a]
list = Gen.list
