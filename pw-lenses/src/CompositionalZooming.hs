{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
-- | https://www.well-typed.com/blog/2018/09/compositional-zooming/
module CompositionalZooming where

import           Control.Lens         (Lens', at, set, (&), (.~), (?~), (^.))
import           Control.Monad.Except (Except, MonadError, throwError)
import           Control.Monad.Reader (MonadReader, ReaderT (ReaderT),
                                       runReaderT)
import           Control.Monad.State  (MonadState, State, StateT (StateT),
                                       modify, runStateT)
import           Data.Biapplicative   (Biapplicative, bipure, (<<*>>))
import           Data.Bifunctor       (Bifunctor, bimap)
import           Data.Coerce          (coerce)
import           Data.Map             (Map)
import qualified Data.Map             as Map

-- * Warming up

-- | Our own implementation of Zoom.
mZoom :: forall st st' a . Lens' st st' -> State st' a -> State st a
mZoom l smallSt = StateT $ \large -> do
  (a, s) <- runStateT smallSt (large ^. l)
  return (a, large & l .~ s)

-- ** Dealing with failures

-- | Version of mZoom that returns a maybe if the inner state is not present.
mZoomM
  :: Lens' st (Maybe st')
  -> State st' a
  -> State st (Maybe a)
mZoomM l smallSt = StateT $ \large ->
  case large ^. l of
    Nothing -> undefined
    Just small -> do
      (a, s) <- runStateT smallSt small
      return (Just a, large & l ?~ s)

-- * Defining our custom data types

newtype Update e st a = Update
  { runUpdate :: StateT st (Except e) a
  } deriving (Functor, Applicative, Monad, MonadState st, MonadError e)

newtype Query e st a = Query
  { runQuery :: ReaderT st (Except e) a
  } deriving (Functor, Applicative, Monad, MonadReader st, MonadError e)

class Biapplicative (Result z) => Zoomable z where
  type Result z :: * -> * -> *

  wrap :: (st -> Result z a st) -> z st a
  unwrap :: z st a -> (st -> Result z a st)

-- ** Zoomable instance for Update

newtype UpdateResult e a st = UpdateResult
  { getUpdateResult :: Except e (a, st) }

instance Bifunctor (UpdateResult e) where
  bimap fa fst (UpdateResult exM) = UpdateResult $ fmap (bimap fa fst) exM

instance Biapplicative (UpdateResult e) where
  bipure a st = UpdateResult $ pure (a, st)

  -- (<<*>>)
  --   :: UpdateResult e (a -> a') (st -> st')
  --   -> UpdateResult e a st
  --   -> UpdateResult e a' st'
  --
  fupdRes <<*>> updRes = UpdateResult $ do
    (fa, fst) <- getUpdateResult fupdRes
    (a, st)   <- getUpdateResult updRes
    return (fa a, fst st)

instance Zoomable (Update e) where
  type Result (Update e) = UpdateResult e

  -- We use instance signatures (enabled with InstanceSigs) just to be able to
  -- understand the types.
  wrap :: (st -> UpdateResult e a st) -> Update e st a
  -- We could write 'wrap' as follows:
  --
  -- > wrap fst = Update $ StateT $ \st -> getUpdateResult $ fst st
  --
  -- But this is what 'coerce' does!
  wrap = coerce

  unwrap :: Update e st a -> (st -> UpdateResult e a st)
  -- unwrap upd st = UpdateResult $ runStateT (runUpdate upd) st
  unwrap = coerce

newtype QueryResult e a st = QueryResult
  { getQueryResult :: Except e a }

instance Bifunctor (QueryResult e) where
  bimap fa _ (QueryResult ex) = QueryResult (fa <$> ex)

instance Biapplicative (QueryResult ex) where
  bipure a st = QueryResult (pure a)

  QueryResult fex <<*>> QueryResult ex = QueryResult (fex <*> ex)

instance Zoomable (Query e) where
  type Result (Query e) = QueryResult e

  wrap :: (st -> QueryResult e a st) -> Query e st a
  -- wrap fqr = Query $ ReaderT $ \st -> getQueryResult (fqr st)
  wrap = coerce

  unwrap :: (Query e st a) -> (st -> QueryResult e a st)
  -- unwrap = coerce
  unwrap q st = QueryResult $ runReaderT (runQuery q) st


-- We need to be able to use the fact that @Result z a@ must be a @Functor@,
-- however in versions of GHC prior to 8.6 we cannot use quantified
-- constraints.
--
-- So we need to resort to this:

newtype FromBi p a st = WrapBi { unwrapBi :: p a st }

instance Bifunctor p => Functor (FromBi p a) where
  fmap f (WrapBi x) = WrapBi $ bimap id f x -- note that @bimap id == second@


-- | Zoom operator for Zoomable instances
zoomZ :: forall z st st' a . Zoomable z
      => Lens' st st' -> z st' a -> z st a
-- zoomW l z = wrap $ \st -> unwrapBi $
--   fmap (\st' -> st & (l .~ st')) (WrapBi $ unwrap z (st ^. l))
zoomZ l z = wrap $ \st -> unwrapBi $ l fst' st
  where res :: st -> Result z a st -- Just to understand the types
        res st = unwrapBi $ l fst' st

        fst' :: st' -> FromBi (Result z) a st'
        fst' = WrapBi . unwrap z
-- You have to remember that:
--
-- > Lens' st st'
-- > == -- def. of Lens' and Lens
-- > forall f . Functor f => (st' -> f st') -> st -> f st
--
-- So given that:
--
-- > unwrap z :: st' -> Result z a st'
--
-- And
--
-- > WrapBi :: p a st' -> FromBi p a st'
--
-- > WrapBi . unwrap z :: st' -> FromBi (Result z) a st'
--
-- So we instantiate the type of the lens @l@ to:
--
-- > l :: st' -> FromBi (Result z) a st'
--

-- | To define zoomZM we need the following function:
liftMaybe
  :: Biapplicative p
  => (st -> p a st) -> Maybe st -> p (Maybe a) (Maybe st)
liftMaybe _ Nothing   = bipure Nothing Nothing
liftMaybe f (Just st) = bimap Just Just $ f st

zoomZM
  :: forall z st st' a . Zoomable z
  => Lens' st (Maybe st')
  -> z st' a
  -> z st (Maybe a)
zoomZM l z = wrap $ \st -> unwrapBi $ l fst' st
  where fst' :: Maybe st' -> FromBi (Result z) (Maybe a) (Maybe st')
        fst' = WrapBi . liftMaybe (unwrap z)


-- | Variation of 'zoomZM' that uses a fallback when the smaller context was
-- not found.
zoomDef
  :: (Zoomable z, Monad (z st))
  => Lens' st (Maybe st')
  -> z st a  -- ^ When not found.
  -> z st' a -- ^ When found.
  -> z st a
zoomDef l def k = zoomZM l k `catchNothing` def
  where
    catchNothing :: Monad m => m (Maybe a) -> m a -> m a
    catchNothing m fallback = m >>= maybe fallback return

-- * Using the combinators.

-- ** Setup

type DB = Map WalletId Wallet
type Wallet = Map AccIx Account
type Account = Map AddrIx Address
type Address = (String, Bool)

type AccIx = Int
type AddrIx = Int

type WalletId = Int
type AccId = (WalletId, AccIx)
type AddrId = (AccId, AddrIx)

-- Note that
--
-- > AddrId = ((WalletId, AccIx), AddrIx)
--
-- So this means that you have all the information you need in the 'AddrId'.

-- *** A hierarchy of errors

data UnknownWallet = UnknownWalletId WalletId

data UnknownAcc = UnknownAccId AccId
                | UnknownAccParent UnknownWallet

data UnknownAddr = UnknownAddrId AddrId
                 | UnknownAddrParent UnknownAcc

-- ** Zooming

zoomWallet
  :: forall e a
  . (UnknownWallet -> e)
  -> WalletId
  -> Update e Wallet a
  -> Update e DB     a
zoomWallet embedErr wId wUpdate = zoomDef (at wId) err wUpdate
  where
    err :: Update e DB a
    err = throwError (embedErr $ UnknownWalletId wId)

-- ** Composition

-- Compositional of the new combinators in this context means that we can
-- define `zoomAccount` in terms of `zoomWallet`

zoomAccount
  :: forall e a
  . (UnknownAcc -> e)
  -> AccId
  -> Update e Account a
  -> Update e DB      a
zoomAccount embedErr aId@(wId, aIx) aUpdate = zoomWallet embedErr' wId wUpdate
  where
    embedErr' :: UnknownWallet -> e
    embedErr' = embedErr . UnknownAccParent

    wUpdate :: Update e Wallet a
    wUpdate = zoomDef (at aIx) err aUpdate

    err :: Update e Wallet a
    err = throwError . embedErr . UnknownAccId $ aId

zoomAddress
  :: forall e a
  .  (UnknownAddr -> e)
  -> AddrId
  -> Update e Address a
  -> Update e DB      a
zoomAddress embedErr addrId@(accId, addrIx) addrUpdate =
  zoomAccount embedErr' accId accUpdate
  where
    embedErr' :: UnknownAcc -> e
    embedErr' = embedErr . UnknownAddrParent

    accUpdate :: Update e Account a
    accUpdate = zoomDef (at addrIx) err addrUpdate

    err :: Update e Account a
    err = throwError . embedErr . UnknownAddrId $ addrId

-- ** Tying everything together

-- Let's write the definition from the introduction.
setUnused :: AddrId -> Update UnknownAddr DB ()
setUnused addrId =
  zoomAddress id addrId addrUpdate
  where
    addrUpdate :: Update UnknownAddr Address ()
    -- addrUpdate = wrap $ \(addrStr, _) -> UpdateResult (pure ((), (addrStr, False)))
    -- The above is not needed because 'Update' is a 'MonadState'!!!
    addrUpdate = modify $ \(addr, _isUsed) -> (addr, False)
