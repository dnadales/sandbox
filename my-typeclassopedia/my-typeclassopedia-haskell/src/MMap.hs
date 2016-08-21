-- |

module MMap where

import qualified Data.Map       as Map
import qualified Data.Monoid    as M
import           Data.Semigroup

newtype MMap k v  = MkMMap (Map.Map k v)
  deriving (Eq, Show)

instance (Ord k, Semigroup v) => Semigroup (MMap k v) where
  (MkMMap m0) <> (MkMMap m1) = MkMMap $ Map.unionWith (\x y -> x <> y) m0 m1

(!) :: (Ord k) => MMap k v -> k -> v
(MkMMap m0) ! k = m0 Map.! k


instance (Ord k, Semigroup v) => Monoid (MMap k v) where
  mempty = MkMMap Map.empty
  mappend = (<>)
