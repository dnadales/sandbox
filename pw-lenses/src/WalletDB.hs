-- | Use the ideas at 'CompositionalZooming' but using combinators from the
-- lens library.

module WalletDB where

import           Control.Monad.Except (Except)
import           Control.Monad.Reader (ReaderT (ReaderT))
import           Control.Monad.State  (StateT (StateT))
import           Data.Map             (Map)

-- * The problem

-- Consider the following definition of an in memory database.

-- | A database maps wallet id's onto wallets.
type DB = Map WalletId Wallet

-- | A wallet associates account indices to accounts.
type Wallet = Map AccIx Account

-- | An account maps addresses indices to addresses.
type Account = Map AddrIx Address

-- | A wallet address.
data Address = Address { _addrName    :: String
                       -- ^ Name of the address.
                       , _addrsActive :: Bool
                       -- ^ Is the address active?
                       } deriving (Show, Eq)

-- | Account index.
type AccIx = Int

-- | Address index.
type AddrIx = Int

-- | Wallet id.
type WalletId = Int

-- | Account id.
type AccId = (WalletId, AccIx)

-- | Address id.
type AddrId = (AccId, AddrIx)

-- | State monad in which we'll perform our updates:
type Update e st a  = StateT st (Except e) a

-- Problem 0: Define the function 'setUnused'
--
-- > setUnused :: AddrId -> Update UnknownAddr DB ()
--
-- That sets the address with the given id (if any) to unused.

-- Problem 1: Define the function 'emptyAllAccounts' that empties all accounts
-- in the DB.
--
-- > emptyAllAccounts :: AddrId -> Update e DB ()
--


