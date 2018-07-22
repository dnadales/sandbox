-- | Extension of the deserialization to include multiplication.
module DeserializationExt where

import           Control.Monad   (liftM2)
import           Data.Foldable   (traverse_)
import           Data.Function   (fix)

import           Deserialization hiding (fromTree, fromTreeExt)
import qualified Deserialization as D
import           ExpSYM
import           MulSYM

fromTreeExt :: (ExpSYM repr, MulSYM repr)
            => (Tree -> Either ErrMsg repr) -> (Tree -> Either ErrMsg repr)
fromTreeExt self (Node "Mul" [e0, e1]) = liftM2 mul (self e0) (self e1)
fromTreeExt self e                     = D.fromTreeExt self e -- NOTE how here the old deserializer is used

-- TODO: note that this definition is also equivalent to the one obtained if
-- you would have covered all the cases in this module, using a standard
-- recursion scheme.

-- | The defintion of 'fromTreeExt' requires the declaraion of a 'MulSYM'
-- instance for 'Tree'.
instance MulSYM Tree where
    mul t0 t1 = Node "Mul" [t0, t1]

instance (MulSYM repr, MulSYM repr') => MulSYM (repr, repr') where
    mul (le0, le1) (re0, re1) = (mul le0 re0, mul le1 re1)

fromTree :: (ExpSYM repr, MulSYM repr) => Tree -> Either ErrMsg repr
fromTree = fix fromTreeExt

-- Now we can use the new interpreter:
tf1Int3 = traverse_ thrice (fromTree tf1Tree)
tfm1Int3 = traverse_ thrice $ fromTree $ toTree tfm1
