{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Use the ideas at 'CompositionalZooming' but using combinators from the
-- lens library.
module WalletDB where

import           Control.Arrow        (second)
import           Control.Lens         (Lens', at, makeLenses, zoom, (.~))
import           Control.Monad.Except (Except, runExcept, throwError)
import           Control.Monad.Reader (ReaderT (ReaderT))
import           Control.Monad.State  (StateT (StateT), modify, put, runStateT)
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

-- Now, lets define a function that sets the state of an address to unused.
setUnusedAddr :: Update e Address ()
setUnusedAddr = modify (used .~ False)

-- Now lets see if we can set a given address index in an account to unused.
setUnusedAcc :: AddrIx -> Update UnknownEntity Account ()
setUnusedAcc aIx = zoom (at aIx) (setUnusedAddr `orElseThrow` UnknownAddr aIx)

-- But it is more general if you do it like this (note that we need to
-- introduce the `UnknownAddr -> e` function to be able to be general about the
-- error type thrown by the update operation on the address):

-- | Lift an update from an Address to the account that contain this address
-- (if any).
updateAccount
  :: AddrIx
  -> Update e Address ()
  -> (UnknownEntity -> e)
  -> Update e Account ()
updateAccount aIx up err =
  zoom (at aIx) (up `orElseThrow` err (UnknownAcc aIx))

updateWallet
  :: AccIx
  -> Update e Account ()
  -> (UnknownEntity -> e)
  -> Update e Wallet ()
updateWallet aIx up err=
  -- zoom (at aIx) (up `orElseThrow` err (UnknownAcc aIx))
  liftUpdate (at aIx) up (err (UnknownAcc aIx))

-- And we can generalize this even further:

-- | Lift an update on the smaller state 'infraSt' to an update on the
-- 'supraState'.
liftUpdate
  :: Lens' supraSt (Maybe infraSt)
  -> Update e infraSt a
  -> e
  -> Update e supraSt a
liftUpdate l up err = zoom l (up `orElseThrow` err)

-- Ok, here I realized that in the original blog post:
--
-- > AddrId = ((WalletId, AccIx), AddrIx)
--
-- So in
--
-- > setUnused :: AddrId -> Update UnknownEntity DB ()
--
-- You have all the information you need to navigate to the address you want to
-- set as unused.
--
-- Also, in each @zoomX@ (@zoomWallet@, @zoomAccount@) the type of the state in
-- the return type is always @DB@, which prevents you from having to compose
-- all the updates at once. In our current solution we'd need to use all the
-- updates functions if we want to define a function like the one below:
--
-- > setUnusedInDB
-- >   :: WalletId
-- >   -> AccIx
-- >   -> AddrIx
-- >   -> Update UnknownEntity DB ()
--

-- ** An attempt with zooming
setUnused
  :: WalletId
  -> AccIx
  -> AddrIx
  -> Update UnknownEntity DB ()
setUnused wId accId addrId =
  zoomAddress wId accId addrId setUnusedAddr

withDB
  :: DB
  -> Update UnknownEntity DB a
  -> Either UnknownEntity (a, DB)
withDB db up =
  runExcept $ runStateT up db

-- Try this out:
--
-- >>> withDB mDB (WalletDB.setUnused "wallet-0" 0 1)
-- >>> withDB mDB (WalletDB.setUnused "wallet-0" 10 1)

zoomAddress
  :: WalletId
  -> AccIx
  -> AddrIx
  -> Update UnknownEntity Address ()
  -> Update UnknownEntity DB ()
zoomAddress wId accIx addrIx up =
  zoomAccount wId accIx (liftUpdate (at addrIx) up err)
  where
    err = UnknownAddr addrIx

zoomAccount
  :: WalletId
  -> AccIx
  -> Update UnknownEntity Account ()
  -> Update UnknownEntity DB ()
zoomAccount wId accIx up =
  zoomWallet wId (liftUpdate (at accIx) up err)
  where
    err = UnknownAcc accIx

zoomWallet
  :: WalletId
  -> Update UnknownEntity Wallet ()
  -> Update UnknownEntity DB ()
zoomWallet wId up =
  liftUpdate (at wId) up err
  where
    err = UnknownWallet wId

orElseThrow :: Update e st a -> e -> Update e (Maybe st) a
orElseThrow up e = StateT $ \mst ->
  case mst of
    Nothing -> throwError e
    Just st -> second Just <$> runStateT up st

-- | Errors arising from keys not being found. For now we aggregate everything into one error type.
data UnknownEntity = UnknownAddr AddrIx
                   | UnknownAcc AccIx
                   | UnknownWallet WalletId
                   deriving (Show)

emptyAllWallets
  :: Update e DB ()
emptyAllWallets =
  zoom traverse (put Map.empty)

-- Try this out
--
-- >>> withDB mDB (emptyAllWallets)
-- Right ((),fromList [("wallet-0",fromList []),("wallet-1",fromList [])])
--
