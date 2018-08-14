-- | Account transactions.

module ChimericLedgers.Transaction.Account where

import           Control.Monad           (mfilter)

import           ChimericLedgers.Address
import           ChimericLedgers.Value

-- | Account based transaction.
data AccTx = AccTx
    { -- | To allow transactions that create money and assign it to the
      -- receiver the sender address is optional.
      sender   :: Maybe Address
      -- | To allow for transaction that take money from the sender and spend
      -- it as fee the receiver address is optional.
    , receiver :: Maybe Address
    , value    :: Value
    , forge    :: Value
    , fee      :: Value
      -- | To prevent replay attacks a transaction. Without this field nothing
      -- prevents the receiver from an authorized transaction to include it
      -- multiple times in the ledger.
    , nonce    :: Int
    } deriving (Eq, Ord, Show)

-- | Validity condition on an account based transaction.
--
--
-- We have the following cases:
--
-- - Both sender and receiver are undefined. In which case the validity
-- condition simplifies to:
--
-- > valid t = forge t == fee t
--
-- which indicates that the money forged in the transaction must correspond to
-- the fee.
--
-- - Sender is undefined, receiver is defined. In which case the validity
-- condition simplifies to:
--
-- > valid t = forge t == fee t + value t
--
-- which is equivalent to:
--
-- > valid t = forge t - fee t == value t
--
-- which indicates that the money transferred to the receiver must be the money
-- forged in the transaction minus the fees (since the money does not come from
-- any account).
--
-- - Sender is defined, receiver is undefined. In which the validity condition
--   simplifies to:
--
-- > valid t = value t = 0
--
-- which indicates that no value can be assigned to the transaction (since
-- there is no recipient), and only money can be forged and fees can be paid.
--
-- - Sender is defined, receiver is defined. In which the validity condition
--   simplifies to:
--
-- > valid t = forge t + value t + fee t - forge t = fee t + value t
--
-- which simplifies to
--
-- > valid t = value t = value t
--
-- which is trivially try, indicating no restriction on a transaction to be
-- valid, if both sender and receiver are defined.
--
-- TODO: QUESTION: Does it make sense to use smart constructors? My fear is
-- that they will make the models a bit more cumbersome.
valid :: AccTx -> Bool
valid t = forge t + spendableMoney == fee t + spentMoney
    where
      spendableMoney = maybe 0 (const (value t + fee t - forge t)) (sender t)
      spentMoney = maybe 0 (const (value t)) (receiver t)

-- | Balance of an account based transaction.
βAcc :: Address -> AccTx -> Value
βAcc a t = received - spent
    where
      spent = (value t + fee t - forge t) `whenAddressEquals` sender
      -- maybe 0 (const (value t + fee t - forge t)) (mfilter (a ==) (sender t))
      received = value t `whenAddressEquals` receiver
      -- maybe 0 (const (value t)) (mfilter (a ==) (receiver t))
      whenAddressEquals val who = maybe 0 (const val) (mfilter (a ==) (who t))

t1 :: AccTx
t1 = AccTx Nothing (Just 1) 1000 1000 0 0

x = βAcc 1 t1

t2 :: AccTx
t2 = AccTx (Just 1) (Just 2) 800 0 0 0

t3 :: AccTx
t3 = AccTx (Just 1) (Just 3) 199 0 1 0

t4 :: AccTx
t4 = AccTx (Just 3) (Just 2) 207 10 2 0

t5 :: AccTx
t5 = AccTx (Just 2) (Just 3) 500 0 7 0

t6 :: AccTx
t6 = AccTx (Just 2) (Just 3) 499 0 1 0
