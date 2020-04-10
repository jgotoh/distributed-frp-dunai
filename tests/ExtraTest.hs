{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ExtraTest
  ( bearRiverExtraTests
  )
where

import           FRP.BearRiver
import           FRP.BearRiver.Network.Reactimate
import           Test.Tasty
import           Test.Tasty.HUnit

bearRiverExtraTests :: TestTree
bearRiverExtraTests = tests

tests :: TestTree
tests = testGroup
  "BearRiverExtraTests"
  [ testCase "test whether countAt x starts at x"             testCountAt
  , testCase "test whether FrameNr incrementation is updated" testFrameNrSF
  ]

testCountAt :: Assertion
testCountAt = do

  rs0 <- embed (countAt 0) (replicate 5 ())

  rs0 @?= [0, 1, 2, 3, 4]

  rs1 <- embed (countAt 4) (replicate 5 ())

  rs1 @?= [4, 5, 6, 7, 8]

newtype TestMessage = TM FrameNr
  deriving (Eq, Show)

instance HasFrameAssociation TestMessage where
  getFrame (TM n) = n

testFrameNrSF :: Assertion
testFrameNrSF = do

  let just x = Just $ TM x

  -- frameNrSF :: (HasFrameAssociation netin, Monad m) => FrameNr -> MSF m (Maybe netin) FrameNr
  print "rs0"
  rs0 <- embed (frameNrSF 0) [Nothing :: Maybe TestMessage, Nothing, Nothing]
  rs0 @?= [0, 1, 2]

  print "rs1"
  rs1 <- embed (frameNrSF 2) [just 10, Nothing, Nothing]
  rs1 @?= [10, 11, 12]

  print "rs2"
  rs2 <- embed (frameNrSF 1) [Nothing, just 10, Nothing]
  rs2 @?= [1, 10, 11]

  rs3 <- embed (frameNrSF 3) [Nothing, Nothing, just 10]
  rs3 @?= [3, 4, 10]

  rs4 <- embed (frameNrSF 0) [just 10, just 100, just 1000]
  rs4 @?= [10, 100, 1000]

  rs5 <- embed (frameNrSF 0) [Nothing, just 10, Nothing, just 100, Nothing]
  rs5 @?= [0, 10, 11, 100, 101]

  rs6 <- embed (frameNrSF 0) [Nothing, just 10, just 10, Nothing, Nothing]
  rs6 @?= [0, 10, 10, 11, 12]
