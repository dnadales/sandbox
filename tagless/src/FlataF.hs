{-# LANGUAGE FlexibleInstances #-}
module FlataF where

import           ExpSYM
import           PushNegF hiding (Ctx)

-- | Context that represents whether @e@ is the left (immediate) child of an
-- addition.
data Ctx e = LCA e | NonLCA

instance ExpSYM repr => ExpSYM (Ctx repr -> repr) where
    lit i NonLCA  = lit i
    lit i (LCA e) = add (lit i) e
    neg e NonLCA    = neg (e NonLCA)
    -- Again, at this point we assume that all the negations are flattened down!
    neg e0 (LCA e1) = add (neg (e0 NonLCA)) e1
    add e0 e1 ctx = e0 (LCA (e1 ctx))

flat :: (Ctx e -> e) -> e
flat e = e NonLCA

norm = flat . pushNeg
