
module Main (main) where

import qualified MagicLengthBasic
import qualified MagicLengthBlockChain

import Control.Monad (void)
import qualified Hedgehog               as HH
import qualified Test.QuickCheck        as QC

import           Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Test.Tasty.Hedgehog as Tasty.Hedgehog
import qualified Test.Tasty.QuickCheck as Tasty.QuickCheck

main :: IO ()
main =  defaultMain tests
  -- I also tried this, to discard possible problems introduced by tasty.
  -- void $ HH.check MagicLengthBasic.hhPropNoMagicLength
  -- QC.quickCheck $ MagicLengthBasic.qcPropNoMagicLength
  where
    tests :: TestTree
    tests = testGroup "Magic"
              [ testGroup "Blockchain"
                [ Tasty.Hedgehog.testProperty "Hedgehog" MagicLengthBlockChain.hhPropNoMagicLength
                , Tasty.QuickCheck.testProperty "QuickCheck" MagicLengthBlockChain.qcPropNoMagicLength
                ]
              , testGroup "Basic"
                [ Tasty.Hedgehog.testProperty "Hedgehog" MagicLengthBasic.hhPropNoMagicLength
                , Tasty.QuickCheck.testProperty "QuickCheck" MagicLengthBasic.qcPropNoMagicLength
                ]
              ]
