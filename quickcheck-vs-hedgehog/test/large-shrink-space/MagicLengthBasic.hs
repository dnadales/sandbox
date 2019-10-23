
module MagicLengthBasic where

import qualified Hedgehog               as HH
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import qualified Test.QuickCheck        as QC

magicLength :: Int
magicLength = 200

hhPropNoMagicLength :: HH.Property
hhPropNoMagicLength =
  HH.property $ do
    xs <- HH.forAll $ Gen.list (Range.constant 0 (2 * magicLength)) aGen
    HH.assert $ (length xs <= magicLength || all ((<= magicLength) . length) xs)
                &&
                all (all (<= magicLength)) xs

  where
    aGen :: HH.Gen [Int]
    aGen = Gen.list (Range.constant 0 (2 * magicLength)) intGen
      where
        intGen = Gen.int (Range.linear 0 magicLength)

qcPropNoMagicLength :: QC.Property
qcPropNoMagicLength =
  QC.forAllShrink
    myGenLists
    QC.shrink
    (\xs -> (length xs <= magicLength || all ((<= magicLength) . length) xs)
            &&
            all (all (<= magicLength)) xs
    )

myGenLists :: QC.Gen [[Int]]
myGenLists = do
  n <- QC.choose (0, 2 * magicLength)
  QC.vectorOf n myGenList
  where
    myGenList :: QC.Gen [Int]
    myGenList = do
      n <- QC.choose (0, 2 * magicLength)
      QC.vector n
