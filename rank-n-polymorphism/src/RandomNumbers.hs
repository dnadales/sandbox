{-# LANGUAGE RankNTypes #-}
-- | An example of rank-n-types about random-numbers, extracted from:
--
--     https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html
--

module RandomNumbers where

import           System.Random

data Player = Player { playerName :: String
                     , playerPos  :: (Double, Double)
                     }
            deriving (Eq, Ord, Show)

-- | Types for generator actions.
type GenAction m = forall a. (Random a) => m a
type GenActionR m = forall a. (Random a) => (a, a) -> m a

