module Main where

import Criterion.Main (defaultMain, bgroup, bench, nfIO)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as HH.Gen
import qualified Hedgehog.Range as HH.Range
import qualified Test.QuickCheck as QC
import Test.QuickCheck (arbitrary)

main :: IO ()
main =
  defaultMain
    [ bgroup "Hedgehog"
             [ bench "not my number"
                     (nfIO $ HH.check hhNotMyNumber)
             , bench "my number is not in the chain"
                     (nfIO $ HH.check hhNotMyNumberInChain)
             ]
    , bgroup "QuickCheck"
             [ bench "my number is not in the chain"
                     (nfIO $ QC.quickCheck qcNotMyNumberInChain)
             ]

    ]

hhNumListGen :: HH.Gen [Int]
hhNumListGen = HH.Gen.list (HH.Range.linear 0 100) genInt
  where
    genInt = HH.Gen.int (HH.Range.constant 0 100)

-- | Check that my phone number doesn't appear in any list.
hhNotMyNumber :: HH.Property
hhNotMyNumber =
  HH.property $
  do
    xs <- HH.forAll hhNumListGen
    xs HH./== myNumber

-- | 555-batata
myNumber :: [Int]
myNumber = [5, 5, 5, 2, 2, 8, 2, 8, 2]

hhListsGen :: HH.Gen [[Int]]
hhListsGen = HH.Gen.list (HH.Range.constant 0 100) hhNumListGen

hhChainListsGen :: HH.Gen [[[Int]]]
hhChainListsGen = HH.Gen.list (HH.Range.constant 0 100) hhListsGen

hhNotMyNumberInChain :: HH.Property
hhNotMyNumberInChain =
  HH.property $
  do
    txss <- HH.forAll hhChainListsGen
    HH.assert $ myNumber `notElem` concat txss

newtype Chain = Chain { unChain :: [Block] }
  deriving (Eq, Show)

newtype Block = Block { unBlock :: [Payload] }
  deriving (Eq, Show)

newtype Payload = Payload { unPayload :: [Int] }
  deriving (Eq, Show)

instance QC.Arbitrary Chain where
  arbitrary = Chain <$> QC.vectorOf 100 arbitrary

instance QC.Arbitrary Block where
  arbitrary = Block <$> QC.vectorOf 100 arbitrary

instance QC.Arbitrary Payload where
  arbitrary = Payload <$> QC.vectorOf 100 arbitrary

qcNotMyNumberInChain :: Chain -> Bool
qcNotMyNumberInChain = ((myNumber `notElem`) . concat) . fmap (fmap unPayload . unBlock) . unChain
