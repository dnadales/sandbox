{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Monad.Trans.Maybe
import Criterion.Main (defaultMain, bgroup, bench, nfIO)
import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.List (foldl')
import Hedgehog (Gen, Range)
import Hedgehog.Internal.Tree (TreeT)

import qualified Hedgehog               as HH
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import qualified Hedgehog.Internal.Tree as Tree
import qualified Test.QuickCheck        as QC

import Manual

import qualified List1
import qualified List2
import qualified List3
import qualified List4

main :: IO ()
main =
  defaultMain
    [ bgroup "Hedgehog: generator"
             [ bench "default" $ nfIO $
                 HH.check $ hhPropChain Gen.list
             , bench "List1" $ nfIO $
                 HH.check $ hhPropChain List1.list
             , bench "List2" $ nfIO $
                 HH.check $ hhPropChain List2.list
             , bench "List3" $ nfIO $
                 HH.check $ hhPropChain List3.list
             , bench "List4" $ nfIO $
                 HH.check $ hhPropChain List4.list
             ]
    , bgroup "QuickCheck: generator"
             [ bench "default" $ nfIO $
                 QC.quickCheck $ QC.withMaxSuccess 1 $ prop . unChain
             ]
    , bgroup "Hedgehog: shrinker"
             [ bench "default" $ nfIO $
                HH.check $ hhPropSeq List3.interleaveList
             , bench "fast" $ nfIO $
                HH.check $ hhPropSeq List3.interleaveListFast
             ]
    , bgroup "QuickCheck: shrinker"
             [ bench "shrink" $ nfIO $
                 QC.quickCheck $ QC.withMaxSuccess 1 $ weirdProp . unSeq
             ]
    ]

{-------------------------------------------------------------------------------
  Hedgehog version
-------------------------------------------------------------------------------}

hhPropChain :: (forall a. Range Int -> Gen a -> Gen [a]) -> HH.Property
hhPropChain list = HH.withTests 1 $ HH.property $ do
    xs <- HH.forAll $ list (Range.singleton listLen) genInt
    HH.assert $ prop xs
  where
    genInt :: HH.Gen Int
    genInt = Gen.int (Range.constant 0 100)

hhPropSeq :: (forall a. [TreeT (MaybeT Identity) a] -> TreeT (MaybeT Identity) [a])
          -> HH.Property
hhPropSeq interleave = HH.withTests 1 $ HH.property $ do
    xs <- HH.forAll $
      fromManual $ fmap interleave $ do
        return $ map (\n -> wrapTreeT $ Just $ Tree.NodeT n []) [1..seqLen]
    HH.assert $ weirdProp xs

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

{-------------------------------------------------------------------------------
  Sequence (for testing the shrinker)
-------------------------------------------------------------------------------}

newtype Seq = Seq { unSeq :: [Int] }
  deriving (Show)

instance QC.Arbitrary Seq where
  arbitrary = return $ Seq [1 .. seqLen]
  shrink    = coerce . QC.shrinkList (const []) . unSeq

isSeq :: [Int] -> Bool
isSeq = and . map (uncurry (==)) . zip [1..]

-- Property designed to bring out the worst in the linear list shrinker
--
-- When we start with a sequence (a list of increasing numbers [1, 2, 3..]),
-- then clearly this does not satisfy @not . isSeq@. However, if we remove
-- anything but a suffix, it /does/ satisfy (i.e., it's no longer a sequence).
-- So a good shrinker will try to remove as large as possible chunks from the
-- /end/ of the list.
--
-- To avoid the shrinker just dropping down to the empty list immediately, we
-- add this as an additional condition.
weirdProp :: [Int] -> Bool
weirdProp xs = null xs || not (isSeq xs)

seqLen :: Int
seqLen = 400
