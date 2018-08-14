{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TypeOperators              #-}
module ChimericLedgers.Transaction where

import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Data.Word (Word32)

data UtxoTx = UtxoTx
    { inputs  :: Set Input
    , outputs :: [Output]
    , forge   :: Value
    , fee     :: Value
    } deriving (Eq, Ord, Show)

newtype Id = Id UtxoTx
    deriving (Eq, Ord, Show)

-- | @hash c@ is (should be) the cryptographic collision-resistant hash of
-- object @c@.
--
hash :: UtxoTx -> Id
hash = Id

inputsList :: UtxoTx -> [Input]
inputsList = Set.toList . inputs

data Input = Input
    { -- | Id of the previous transaction to which this input refers.
      tid   :: Id
      -- | Index within the transaction referred by 'tid' that refers to the
      -- output that should be spent.
    , index :: Int
    } deriving (Eq, Ord, Show)

(@@) :: UtxoTx -> Int -> Input
t @@ i = Input (hash t) i

data Output = Output
    { -- | Address that owns the value.
      address :: Address
    , value   :: Value
    } deriving (Eq, Ord, Show)

($-->@) :: Value -> Address -> Output
v $-->@ a = Output a v

newtype Value = Value Word32
    deriving (Eq, Ord, Num, Show)

newtype Address = Address Word32
    deriving (Eq, Ord, Num, Show)

-- | Calculate the unspent outputs, by calculating a set of inputs that hold a
-- reference to the unspent transactions.
unspentOuts :: UtxoTx -> Set Input
unspentOuts t = Set.fromList $ fmap (uncurry Input) $ zip tHashes [0..]
    where tHashes = replicate (length (outputs t)) (hash t)

-- | Outputs spent by a transaction. This is just the set of inputs.
spentOuts :: UtxoTx -> Set Input
spentOuts = inputs

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

t1 :: UtxoTx
t1 = UtxoTx
    { inputs = []
    , outputs = [1000 $-->@ 1]
    , forge = 1000
    , fee = 0
    }

t2 :: UtxoTx
t2 = UtxoTx
    { inputs = [t1 @@ 0]
    , outputs = [800 $-->@ 2, 200 $-->@ 1]
    , forge = 0
    , fee = 0
    }

t3 :: UtxoTx
t3 = UtxoTx
    { inputs = [t2 @@ 1]
    , outputs = [199 $-->@ 3]
    , forge = 0
    , fee = 1
    }


t4 :: UtxoTx
t4 = UtxoTx
    { inputs = [t3 @@ 0]
    , outputs = [207 $-->@ 2] -- The total amount we can spend is: 199 + 10 - 2
    , forge = 10 -- On this model, the money that is forged can be spent on any output.
    , fee = 2
    }


t5 :: UtxoTx
t5 = UtxoTx
    { inputs = [t4 @@ 0, t2 @@ 0]
    , outputs = [500 $-->@ 2, 500 $-->@ 3]
    , forge = 0
    , fee = 7
    }

t6 :: UtxoTx
t6 = UtxoTx
    { inputs = [t5 @@ 0, t5 @@ 1]
    , outputs = [999 $-->@ 3]
    , forge = 0
    , fee = 1
    }

-- TODO:
-- Add some tests
-- ghci> isValid [t1, t2, t3, t4, t5] t6
--
-- [t1, t2, t3, t4, t5 t6]

