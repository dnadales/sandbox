{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module MulSYM where

import           ExpSYM
import qualified ExpSYM

class MulSYM repr where
    mul :: repr -> repr -> repr

tfm1 :: (ExpSYM repr, MulSYM repr) => repr
tfm1 = add (lit 7) (neg (mul (lit 1) (lit 2)))

tfm2 :: (ExpSYM repr, MulSYM repr) => repr
tfm2 = mul (lit 7) ExpSYM.tf1

--------------------------------------------------------------------------------
-- Instances of @MulSYM@
--------------------------------------------------------------------------------

instance MulSYM Int where
    mul = (*)

instance MulSYM String where
    mul e0 e1 = "(" ++ e0 ++ " * " ++ e1 ++ ")"

