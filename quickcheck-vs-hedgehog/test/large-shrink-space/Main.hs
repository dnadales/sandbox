
module Main (main) where

import qualified Hedgehog               as HH
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

main :: IO ()
main = HH.check hhPropNoMagicLength >>= print

magicLength :: Int
magicLength = 50

hhPropNoMagicLength :: HH.Property
hhPropNoMagicLength =
  HH.property $ do
    xs <- HH.forAll $ Gen.list (Range.constant 0 (2 * magicLength)) aGen
    HH.assert $ length xs <= magicLength

  where
    aGen :: HH.Gen [Int]
    aGen = Gen.list (Range.constant 0 1000) intGen
      where
        intGen = Gen.int (Range.linear 0 100)
