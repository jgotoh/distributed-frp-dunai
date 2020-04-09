{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ClientSidePredictionTest
  ( clientSidePredictionTests
  )
where

import           Data.MonadicStreamFunction
                                         hiding ( trace )
import           Data.MonadicStreamFunction.Network.Prediction
import           Test.Tasty
import           Test.Tasty.HUnit

clientSidePredictionTests :: TestTree
clientSidePredictionTests = tests

tests :: TestTree
tests =
  testGroup "ClientSidePredictionTest" [testCase "test prediction" testPredict]

double :: Monad m => MSF m Int Int
double = arr (* 2)

mult :: Monad m => Int -> MSF m Int Int
mult x = arr (* x)

-- Tests predict function. Tests whether a simulating sf is correctly used when input to MSF is Nothing. Also tests whether a switched into MSF is used when the input is defined.
-- When a defined input arrives, it is first directly output without simulating (which means input to 'double' here is ignored). Only on the next application, the switched into MSF is applied.
testPredict :: Assertion
testPredict = do

  let predict' = predict double mult

  -- no Just values -> double is applied at all times
  let in0 =
        [(1, Nothing), (2, Nothing), (3, Nothing), (5, Nothing), (8, Nothing)]
      rs0 = [2, 4, 6, 10, 16]

  rs0' <- embed predict' in0
  rs0' @?= rs0

  -- switch once
  let in1 =
        [(1, Nothing), (2, Just 3), (5, Nothing), (8, Nothing), (16, Nothing)]
      rs1 = [2, 3, 15, 24, 48]
  rs1' <- embed predict' in1
  rs1' @?= rs1

  -- switch twice
  let in2 =
        [(1, Nothing), (2, Just 3), (5, Nothing), (8, Just 16), (2, Nothing)]
      rs2 = [2, 3, 15, 16, 32]
  rs2' <- embed predict' in2
  rs2' @?= rs2

