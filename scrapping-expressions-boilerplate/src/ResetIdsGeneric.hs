{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ResetIdsGeneric where

import           GHC.Generics

import           ResetIds     hiding (id, (~~))

-- | Expressions that support a reset on all the @Id@ values inside it.
class Reset e where
    -- | Reset all the @Id@'s inside an expression.
    reset :: e -> e

    default reset :: (Generic e, GReset (Rep e)) => e -> e
    reset = to . greset . from
    -- Remember that:
    --
    -- > from  :: a -> (Rep a) x

class GReset f where
    greset :: f a -> f a

-- | Resetting a constructor without arguments will give the same result, since
-- no @Id@ can be found there.
instance GReset U1 where
    greset U1 = U1

-- | Resetting the product is equal to resetting each term in the product.
instance (GReset a, GReset b) => GReset (a :*: b) where
    greset (a :*: b) = greset a :*: greset b

-- | Resetting the sum amounts to resetting the each term of it as well.
instance (GReset a, GReset b) => GReset (a :+: b) where
    greset (L1 x) = L1 (greset x)
    greset (R1 x) = R1 (greset x)

-- | We do need to do anything for resetting the meta-data.
instance (GReset a) => GReset (M1 i c a) where
    greset (M1 x) = M1 (greset x)

-- | And the only interesting case: resetting the arguments of the type
-- constructors. In this case we have to use our @Reset@ (__not @GReset@__).
instance (Reset a) => GReset (K1 i a) where
    greset (K1 x) = K1 (reset x)

-- * And now we finally have the elements in place for scrapping the boilerplate!
instance Reset Id where
    reset (Id _) = Id 0

instance Reset Char where
    reset = id

instance (Reset a) => Reset [a]

instance Reset VarId

instance Reset SortId

instance Reset FuncId

instance Reset Exp

(~~) :: (Reset e, Eq e) => e -> e -> Bool
e0 ~~ e1 = reset e0 == reset e1

test0 = print $ exp0 ~~ exp1
test1 = print $ exp0 ~~ exp0'
