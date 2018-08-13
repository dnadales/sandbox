module ChimericLedgers.Ledger where

import           Data.List                   (foldl')
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           ChimericLedgers.Id
import           ChimericLedgers.Transaction hiding (unspentOutputs)
import qualified ChimericLedgers.Transaction as T

newtype Ledger = Ledger [Transaction]

--------------------------------------------------------------------------------
-- Functions on input, ledgers, and transactions.
--------------------------------------------------------------------------------

-- | @tx i xs@ retrieves a transaction @t@ contained in @xs@ such that:
--
-- > hash t = tid i
--
-- See also 'tid'.
tx :: Input -> Ledger -> Maybe UtxoTx
tx = undefined

-- | Retrieves the output referred by the given input.
--
-- If @tx i = Just t@, then
--
-- > out i = Just $ inputs t !! index i
out :: Input -> Ledger -> Maybe Output
out = undefined

-- | Retrieves the value associated to the given output.
--
-- If @out i = Just o@, then
--
-- > value i = Just $ value o
--
value :: Input -> Ledger -> Maybe Value
value = undefined

-- | Calculate the unspent outputs in a ledger.
unspentOutputs :: Ledger -> Set Input
unspentOutputs (Ledger xs) = foldl' update Set.empty  xs
    where update :: Set Input -> Transaction -> Set Input
          update = undefined
