module ChimericLedgers.Transaction where

import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Word          (Word32)

import           ChimericLedgers.Id

data Transaction

data UtxoTx = UtxoTx
    { inputs  :: Set Input
    , outputs :: [Output]
    , forge   :: Value
    , fee     :: Value
    }

data Input = Input
    { -- | Id of the previous transaction to which this input refers.
      tid   :: Id
      -- | Index within the transaction referred by 'tid' that refers to the
      -- output that should be spent.
    , index :: Int
    } deriving (Eq, Ord, Show)

data Output = Output
    { -- | Address that owns the value.
      address :: Address
    , value   :: Value
    }

type Value = Word32

type Address = Word32

-- | Calculate the unspent outputs, by calculating a set of inputs that hold a
-- reference to the unspent transactions.
unspentOuts :: UtxoTx -> Set Input
unspentOuts t = Set.fromList $ fmap (uncurry Input) $ zip outHashes [0..]
    where outHashes = hash <$> outputs t

-- | Outputs spent by a transaction. This is just the set of inputs.
spentOuts :: UtxoTx -> Set Input
spentOuts = inputs
