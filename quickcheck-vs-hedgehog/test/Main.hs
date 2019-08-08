{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Criterion.Main (defaultMain, bgroup, bench, nfIO)
import Data.Coerce (coerce)
import Data.List (foldl')
import Hedgehog (Gen, Range)

import qualified Hedgehog        as HH
import qualified Hedgehog.Gen    as Gen
import qualified Hedgehog.Range  as Range
import qualified Test.QuickCheck as QC

import qualified List1
import qualified List2
import qualified List3
import qualified List4

main :: IO ()
main =
  defaultMain
    [ bgroup "Hedgehog"
             [ bench "default" $ nfIO $
                 HH.check $ hhProp Gen.list
             , bench "List1" $ nfIO $
                 HH.check $ hhProp List1.list
             , bench "List2" $ nfIO $
                 HH.check $ hhProp List2.list
             , bench "List3" $ nfIO $
                 HH.check $ hhProp List3.list
             , bench "List4" $ nfIO $
                 HH.check $ hhProp List4.list
             ]
    , bgroup "QuickCheck"
             [ bench "default" $ nfIO $
                 QC.quickCheck $ QC.withMaxSuccess 1 $ prop . unChain
             ]
    ]

{-------------------------------------------------------------------------------
  Hedgehog version
-------------------------------------------------------------------------------}

hhProp :: (forall a. Range Int -> Gen a -> Gen [a]) -> HH.Property
hhProp list = HH.withTests 1 $ HH.property $ do
    xs <- HH.forAll $ list (Range.singleton listLen) genInt
    HH.assert $ prop xs
  where
    genInt :: HH.Gen Int
    genInt = Gen.int (Range.constant 0 100)

{-------------------------------------------------------------------------------
  QuickCheck version
-------------------------------------------------------------------------------}

newtype Chain = Chain [QC.NonNegative Int]
  deriving (Show)

instance QC.Arbitrary Chain where
  arbitrary = Chain <$> QC.vectorOf listLen QC.arbitrary

unChain :: Chain -> [Int]
unChain = coerce

{-------------------------------------------------------------------------------
  Shared
-------------------------------------------------------------------------------}

listLen :: Int
listLen = 100000

prop :: [Int] -> Bool
prop xs = foldl' (+) 0 xs >= 0
