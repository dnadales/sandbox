
module Main (main) where

import Data.List (zip4)

import           Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Tasty.Hedgehog as Tasty.Hedgehog
import qualified Test.Tasty.QuickCheck as Tasty.QuickCheck

import qualified Hedgehog               as HH
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import qualified Test.QuickCheck        as QC
import           Test.QuickCheck (Arbitrary, arbitrary, shrink)


main :: IO ()
main = QC.quickCheck qcPropNoMagicLength
  -- defaultMain tests
  -- where
  --   tests :: TestTree
  --   tests = testGroup "Large Shrink Space"
  --             [ Tasty.Hedgehog.testProperty "Hedgehog" hhPropNoMagicLength
  --             , Tasty.QuickCheck.testProperty "QuickCheck" qcPropNoMagicLength
  --             ]

magicLength :: Int
magicLength = 42

payloadLengthMin :: Int
payloadLengthMin = 35

payloadLengthMax :: Int
payloadLengthMax = 45

hhPropNoMagicLength :: HH.Property
hhPropNoMagicLength =
  HH.property $ do
    xs <- HH.forAll $ Gen.list (Range.constant 0 (2 * magicLength)) blockGen
    HH.assert $ length xs <= magicLength
  where
    blockGen :: HH.Gen Block
    blockGen =  Block
            <$> Gen.list payloadRange txGen
            <*> Gen.list payloadRange delegationGen
            <*> Gen.list payloadRange updateGen
            <*> Gen.list payloadRange voteGen
      where
        payloadRange = Range.constant payloadLengthMin payloadLengthMax

        txGen :: HH.Gen Transaction
        txGen = Transaction <$> Gen.list payloadRange ioGen
          where
            ioGen :: HH.Gen InputOutput
            ioGen =  InputOutput
                  <$> Gen.list (Range.linear 1 5) inputGen
                  <*> Gen.list (Range.linear 1 5) outputGen
              where
                inputGen :: HH.Gen Input
                inputGen = Input <$> intGen

                outputGen :: HH.Gen Output
                outputGen = Output <$> intGen

        intGen :: HH.Gen Int
        intGen = Gen.int (Range.linear 0 100)

        delegationGen :: HH.Gen Delegation
        delegationGen = Delegation <$> intGen

        updateGen :: HH.Gen Update
        updateGen = Update <$> Gen.list payloadRange intGen

        voteGen :: HH.Gen Vote
        voteGen = Vote <$> Gen.int (Range.linear 0 100)

data  Block = Block [Transaction] [Delegation] [Update] [Vote]
  deriving (Eq, Show)

newtype Transaction = Transaction [InputOutput]
  deriving (Eq, Show)

newtype Delegation = Delegation Int
  deriving (Eq, Show)

newtype Update = Update [Int]
  deriving (Eq, Show)

newtype Vote = Vote Int
  deriving (Eq, Show)

data InputOutput = InputOutput [Input] [Output]
  deriving (Eq, Show)

newtype Input = Input Int
  deriving (Eq, Show)

newtype Output = Output Int
  deriving (Eq, Show)

qcPropNoMagicLength :: [Block] -> Bool
qcPropNoMagicLength xs = length xs <= magicLength

instance Arbitrary Block where
  arbitrary = Block
            <$> listWithLengthRange payloadLengthMin payloadLengthMax arbitrary
            <*> listWithLengthRange payloadLengthMin payloadLengthMax arbitrary
            <*> listWithLengthRange payloadLengthMin payloadLengthMax arbitrary
            <*> listWithLengthRange payloadLengthMin payloadLengthMax arbitrary

  shrink (Block ts ds us vs) =
    [ Block ts' ds' us' vs'
    | (ts', ds', us', vs') <- zip4 (shrinkListUpTo payloadLengthMin ts)
                                   (shrinkListUpTo payloadLengthMin ds)
                                   (shrinkListUpTo payloadLengthMin us)
                                   (shrinkListUpTo payloadLengthMin vs)
    ]


listWithLengthRange :: Int -> Int -> QC.Gen a -> QC.Gen [a]
listWithLengthRange minLength maxLength genA = do
  n <- QC.choose (minLength, maxLength)
  QC.vectorOf n genA

shrinkListUpTo :: Arbitrary a => Int -> [a] -> [[a]]
shrinkListUpTo minLength xs =
  filter ((minLength <=) . length) [ xs' | xs' <- shrink xs ]

instance Arbitrary Transaction where
  arbitrary =
    Transaction <$> listWithLengthRange payloadLengthMin payloadLengthMax arbitrary

  shrink (Transaction ios) =
    [ Transaction ios' | ios' <- shrinkListUpTo payloadLengthMin ios ]

instance Arbitrary InputOutput where
  arbitrary = InputOutput <$> arbitrary <*> arbitrary

  shrink (InputOutput is os) =  [ InputOutput is' os' | (is', os') <- shrink (is, os) ]

instance Arbitrary Input where
  arbitrary = Input <$> QC.choose (0, 100)

instance Arbitrary Output where
  arbitrary = Output <$> QC.choose (0, 100)

instance Arbitrary Vote where
  arbitrary = Vote <$> QC.choose (0, 100)

instance Arbitrary Delegation where
  arbitrary = Delegation <$> QC.choose (0, 100)

instance Arbitrary Update where
  arbitrary = Update <$> listWithLengthRange payloadLengthMin payloadLengthMax arbitrary

  shrink (Update us) = [ Update us' | us' <- shrinkListUpTo payloadLengthMin us ]
