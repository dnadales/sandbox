{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies    #-}
module ChimericLedgers.Ledger where

import           Data.List                           (find, foldl')
import           Data.Maybe                          (fromMaybe, mapMaybe)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           GHC.Exts                            (IsList, Item, fromList,
                                                      toList)
import           Safe                                (atMay)

import           ChimericLedgers.Address
import           ChimericLedgers.Transaction         hiding (value)
import qualified ChimericLedgers.Transaction         as T
import           ChimericLedgers.Transaction.Account (AccTx)
import qualified ChimericLedgers.Transaction.Account as Acc
import           ChimericLedgers.Value

newtype Ledger t = Ledger [t]

instance IsList (Ledger t) where
    type Item (Ledger t) = t
    fromList = Ledger
    toList (Ledger ts) = ts

--------------------------------------------------------------------------------
-- Functions on input, ledgers, and transactions.
--------------------------------------------------------------------------------

-- | @tx i xs@ retrieves a transaction @t@ contained in @xs@ such that:
--
-- > hash t = tid i
--
-- See also 'tid'.
tx :: Input -> Ledger UtxoTx -> Maybe UtxoTx
tx i (Ledger ts) = find ((tid i ==) . hash) ts

-- | Retrieves the output referred by the given input.
--
-- If @tx i = Just t@, then
--
-- > out i = Just $ outputs t !! index i
--
out :: Input -> Ledger UtxoTx -> Maybe Output
out i λ = do
    t <- tx i λ
    outputs t `atMay` index i

-- | Retrieves the value associated to the given input.
--
-- If @out i = Just o@, then
--
-- > value i = Just $ value o
--
value :: Ledger UtxoTx -> Input -> Maybe Value
value λ i = T.value <$> out i λ

-- | Calculate the unspent outputs in a ledger.
unspentOutputs :: Ledger UtxoTx -> Set Input
unspentOutputs (Ledger ts) = foldl' update Set.empty ts
    where update :: Set Input -> UtxoTx -> Set Input
          update acc t = (acc `Set.difference` T.spentOuts t)
                         `Set.union` T.unspentOuts t

-- | Is a transaction @t@ valid w.r.t a ledger @λ@?
isValidUTxO :: Ledger UtxoTx -> UtxoTx -> Bool
isValidUTxO λ t = allInputsUnspent && valueIsPreserved
    where
      allInputsUnspent = inputs t `Set.isSubsetOf` unspentOutputs λ
      valueIsPreserved = forge t + sum inputValues == fee t + sum outputValues
      inputValues = mapMaybe (value λ) (inputsList t)
      outputValues = T.value <$> outputs t

-- | Balance of an address @a@ in a valid transaction @t@, w.r.t. a given
-- ledger @λ@.
βUTxO :: Address -> Ledger UtxoTx -> UtxoTx -> Value
βUTxO a λ t = paidOutputs - spentOutputs
    where paidOutputs :: Value -- TODO: what should happen if @paidOutputs < spentOutputs@?
          paidOutputs = sumValues $ paidToA $ outputs t
          spentOutputs = sumValues $ paidToA $ mapMaybe (`out` λ) (inputsList t)
          sumValues = sum . map T.value
          paidToA = filter ((a ==) . address)

-- | Balance of an address @a@ in a given ledger @λ@.
--
-- Since this function and 'βUTxO' have different number of arguments, I cannot
-- get away with defining them in a type-class (which might not be the best
-- modelling decision anyway). If we want to use the same name, we have to
-- define these operations in separate modules.
βUTxOL :: Address -> Ledger UtxoTx -> Value
βUTxOL a λ@(Ledger ts) = foldl' (+) 0 $ map (βUTxO a λ) ts

--------------------------------------------------------------------------------
-- Examples (add them as tests later)
--------------------------------------------------------------------------------
l0 :: Ledger UtxoTx
l0 = [t1, t2, t3, t4, t5, t6]

-- TODO: add this as a test.
res = unspentOutputs l0 == [t6 @@ 0]

res'' = βUTxOL 3 l0 == 999

βAcc :: Address -> Ledger AccTx -> Value
βAcc a (Ledger ts) = foldl' (+) 0 ((a `Acc.βAcc`) <$> ts)

isValidAcc :: Ledger AccTx -> AccTx -> Bool
isValidAcc λ@(Ledger ts) t = senderHasEnoughMoney && t `notElem` ts
    where
      senderHasEnoughMoney = fromMaybe True $ do
          a <- Acc.sender t
          return $ Acc.value t + Acc.fee t - Acc.forge t <= βAcc a λ

isValid01 = isValidAcc [] Acc.t1
isValid02 = isValidAcc [Acc.t1] Acc.t2
isValid03 = isValidAcc [Acc.t1, Acc.t2] Acc.t3
isValid04 = isValidAcc [Acc.t1, Acc.t2, Acc.t3] Acc.t4
isValid05 = isValidAcc [Acc.t1, Acc.t2, Acc.t3, Acc.t4] Acc.t5
isValid06 = isValidAcc [Acc.t1, Acc.t2, Acc.t3, Acc.t4, Acc.t5] Acc.t6

-- TODO: include this in tests
res0 = βAcc 1 [Acc.t1]
res' = βAcc 3 [Acc.t1, Acc.t2, Acc.t3, Acc.t4, Acc.t5, Acc.t6] == 999
