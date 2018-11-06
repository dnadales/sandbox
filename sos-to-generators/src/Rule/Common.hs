{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rule.Common where

import           Numeric.Natural (Natural)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary)

newtype Epoch = Epoch Natural
  deriving (Show, Eq, Ord, Num)

newtype Slot = Slot Natural
  deriving (Show, Eq, Ord, Num)

-- |Representation of the owner of key pair.
newtype Owner = Owner Natural deriving (Show, Eq, Ord)

-- |Signing Key.
newtype SKey = SKey Owner deriving (Show, Eq, Ord)

-- |Verification Key.
newtype VKey = VKey Owner deriving (Show, Eq, Ord)

arbitraryNat :: Gen Natural
arbitraryNat  = fromInteger . abs <$> arbitrary

instance Arbitrary Epoch where
  arbitrary = Epoch <$> arbitraryNat

instance Arbitrary Slot where
  arbitrary = Slot <$> arbitraryNat

instance Arbitrary Owner where
  arbitrary = Owner <$> arbitraryNat

instance Arbitrary SKey where
  arbitrary = SKey <$> arbitrary

instance Arbitrary VKey where
  arbitrary = VKey <$> arbitrary

newtype VKeyGen = VKeyGen VKey
  deriving (Show, Eq, Ord)

instance Arbitrary VKeyGen where
  arbitrary = VKeyGen <$> arbitrary

