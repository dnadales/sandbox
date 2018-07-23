{-# LANGUAGE FlexibleInstances #-}
-- | Extension of the negation pushing with multiplication.
module PushNegFExt where

import           MulSYM
import           PushNegF

instance MulSYM repr => MulSYM (Ctx -> repr) where
    mul e0 e1 Pos = mul (e0 Pos) (e1 Pos)
    --  In the equation below, the second number was choosen to be multiplied
    --  by -1. This choice is arbitrary.
    mul e0 e1 Neg = mul (e0 Pos) (e1 Neg)
