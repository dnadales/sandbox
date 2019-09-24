
module Main (main) where

import qualified MagicLengthBasic
import qualified MagicLengthBlockChain

import           Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Tasty.Hedgehog as Tasty.Hedgehog
import qualified Test.Tasty.QuickCheck as Tasty.QuickCheck

import qualified Test.QuickCheck        as QC
import           Test.QuickCheck (Arbitrary, arbitrary, shrink)


main :: IO ()
main =
  defaultMain tests
  where
    tests :: TestTree
    tests = testGroup "Magic"
              [ testGroup "Blockchain"
                [ Tasty.Hedgehog.testProperty "Hedgehog" MagicLengthBlockChain.hhPropNoMagicLength
                , Tasty.QuickCheck.testProperty "QuickCheck" MagicLengthBlockChain.qcPropNoMagicLength
                ]
              , testGroup "Basic"
                [ Tasty.Hedgehog.testProperty "Hedgehog" MagicLengthBasic.hhPropNoMagicLength
                ]
              ]
