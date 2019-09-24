
module MagicLengthBasic where

import qualified Hedgehog               as HH
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

magicLength :: Int
magicLength = 10

hhPropNoMagicLength :: HH.Property
hhPropNoMagicLength =
  HH.property $ do
    xs <- HH.forAll $ Gen.list (Range.constant 0 (2 * magicLength)) aGen
    HH.assert $ length xs <= magicLength || all ((<= magicLength) . length) xs

  where
    aGen :: HH.Gen [Int]
    aGen = Gen.list (Range.constant 0 (2 * magicLength)) intGen
      where
        intGen = Gen.int (Range.linear 0 100)
