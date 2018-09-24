{-# LANGUAGE TemplateHaskell #-}
-- | Use the ideas at 'CompositionalZooming' but using combinators from the
-- lens library.

module WalletDB where

import           Control.Lens         (at, makeLenses, zoom, (.~))
import           Control.Monad.Except (Except)
import           Control.Monad.Reader (ReaderT (ReaderT))
import           Control.Monad.State  (StateT (StateT), modify)
import           Data.Map             (Map)
import qualified Data.Map             as Map

-- * The problem

-- Consider the following definition of an in memory database.

-- | Account index.
type AccIx = Int

-- | Address index.
type AddrIx = Int

-- | Wallet id.
type WalletId = String

-- | A database maps wallet id's onto wallets.
type DB = Map WalletId Wallet

-- | A wallet associates account indices to accounts.
type Wallet = Map AccIx Account

-- | An account maps addresses indices to addresses.
type Account = Map AddrIx Address

-- | A wallet address.
data Address = Address { _name :: String
                       -- ^ Name of the address.
                       , _used :: Bool
                       -- ^ Is the address used?
                       } deriving (Show, Eq)
makeLenses ''Address

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


-- Let's see if we can define an example database first.

mDB :: DB
mDB = Map.fromList
  [ ("wallet-0", w0)
  , ("wallet-1", w1)
  ]

w0 :: Wallet
w0 = Map.fromList
  [ (0, w0acc0)
  , (1, w0acc1)
  ]

w1 :: Wallet
w1 = Map.fromList
  []

w0acc0 :: Account
w0acc0 = Map.fromList
  [ (0, Address "w0acc0-addr0" True)
  , (1, Address "w0acc0-addr1" True)
  ]

w0acc1 :: Account
w0acc1 = Map.fromList
  [ (0, Address "w0acc1-addr0" True)
  , (1, Address "w0acc1-addr1" True)
  ]


-- *** A hierarchy of errors, again.

-- Now, lets define a function that sets the state of an address to unused.
setUnused :: Update e Address ()
setUnused = modify (used .~ False)


-- Now lets see if we can set a given address index in an account to unused.
setUnusedAcc :: AddrIx -> Update UnknownAddr Account ()
setUnusedAcc aIx = zoom (at aIx) setUnused
  -- I'd need a Lens' Account Address

-- TODO: next: define something like this
-- updateAddr
--   :: AddrIx
--   -> Update e Address ()
--   -> Update UnknownAddr (Maybe Address) ()
-- updateAddr aIx update =


newtype UnknownAddr = UnknownAddr AddrIx
