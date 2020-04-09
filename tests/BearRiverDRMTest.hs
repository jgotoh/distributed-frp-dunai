{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BearRiverDRMTest
  ( bearRiverDRMTests
  )
where

import           Data.Vector2
import           FRP.BearRiver
import           FRP.BearRiver.Network.Prediction
import           FRP.BearRiver.Extra
import           Test.Tasty
import           Test.Tasty.HUnit

bearRiverDRMTests :: TestTree
bearRiverDRMTests = tests

tests :: TestTree
tests = testGroup
  "BearRiverDRMTests"
  [testCase "test drmZero" testDRMZero, testCase "test drmFirst" testDRMFirst]

type Position = Vector2 Double
type Velocity = Vector2 Double
type Acceleration = Vector2 Double

instance HasPosition A Position where
  getPosition = posA

instance HasVelocity A Velocity where
  getVelocity = velA

instance HasAcceleration A Acceleration where
  getAcceleration = accA

data A = A {posA :: Position, velA :: Velocity, accA :: Acceleration}
  deriving (Show, Eq)

newA :: A -> Position -> A
newA a pos = a { posA = pos }

v2 :: RealFloat a => a -> a -> Vector2 a
v2 x y = vector2 x y

dt :: DTime -> Maybe a -> (DTime, Maybe a)
dt = (,)

-- Tests drmZero, never extrapolate values
testDRMZero :: Assertion
testDRMZero = do

  let a0 = A (v2 1 1) (v2 10 10) (v2 100 100)
      test input = embedSF (drmZero a0) input
      rA pos = a0 { posA = pos }

  -- apply to Nothing, return a0
  let in0 = [dt 0 Nothing]
  rs0 <- test in0
  rs0 @?= [a0]

  let in1 = [dt 1 Nothing]
  rs1 <- test in1
  rs1 @?= [a0]

  let in2 = [dt 1 Nothing, dt 0.5 Nothing]
  rs2 <- test in2
  rs2 @?= [a0, a0]

  let in3 = [dt 0 Nothing, dt 0 (Just (rA (v2 20 20)))]
  rs3 <- test in3
  rs3 @?= [a0, rA (v2 20 20)]

  let in4 = [dt 0 Nothing, dt 10 (Just (rA (v2 20 20)))]
  rs4 <- test in4
  rs4 @?= [a0, rA (v2 20 20)]

  let in5 =
        [dt 0 Nothing, dt 10 (Just (rA (v2 20 20))), dt 10 (Just (rA (v2 5 5)))]
  rs5 <- test in5
  rs5 @?= [a0, rA (v2 20 20), rA (v2 5 5)]

  let in6 = [dt 0 Nothing, dt 10 (Just (rA (v2 20 20))), dt 2 Nothing]
  rs6 <- test in6
  rs6 @?= [a0, rA (v2 20 20), rA (v2 20 20)]

  let in7 =
        [dt 0 Nothing, dt 10 (Just (rA (v2 20 20))), dt 2 Nothing, dt 1 Nothing]
  rs7 <- test in7
  rs7 @?= [a0, rA (v2 20 20), rA (v2 20 20), rA (v2 20 20)]

  let in8 =
        [ dt 0  Nothing
        , dt 10 (Just (rA (v2 20 20)))
        , dt 2  Nothing
        , dt 1  Nothing
        , dt 1  (Just (rA (v2 30 30)))
        ]
  rs8 <- test in8
  rs8 @?= [a0, rA (v2 20 20), rA (v2 20 20), rA (v2 20 20), rA (v2 30 30)]

testDRMFirst :: Assertion
testDRMFirst = do

  let a0 = A (v2 1 1) (v2 10 10) (v2 100 100)
      test input = embedSF (drmFirst a0 newA) input
      rA pos = a0 { posA = pos }

  -- apply to Nothing, return a0
  let in0 = [dt 0 Nothing]
  rs0 <- test in0
  rs0 @?= pure a0

  -- dt > 0, so interpolate a0
  let in1 = [dt 1 Nothing]
  rs1 <- test in1
  rs1 @?= [rA (v2 11 11)]

  -- interpolate a0 twice
  let in2 = [dt 1 Nothing, dt 0.5 Nothing]
  rs2 <- test in2
  rs2 @?= [rA (v2 11 11), rA (v2 16 16)]

  let in3 = [dt 0 Nothing, dt 0 (Just (rA (v2 20 20)))]
  rs3 <- test in3
  rs3 @?= [a0, rA (v2 20 20)]

  -- if a Just value comes with a dt of > 0, it must not be extrapolated
  let in4 = [dt 0 Nothing, dt 10 (Just (rA (v2 20 20)))]
  rs4 <- test in4
  rs4 @?= [a0, rA (v2 20 20)]

  let in5 =
        [dt 0 Nothing, dt 10 (Just (rA (v2 20 20))), dt 10 (Just (rA (v2 5 5)))]
  rs5 <- test in5
  rs5 @?= [a0, rA (v2 20 20), rA (v2 5 5)]

  let in6 = [dt 0 Nothing, dt 7 (Just (rA (v2 20 20))), dt 2 Nothing]
  rs6 <- test in6
  rs6 @?= [a0, rA (v2 20 20), rA (v2 40 40)]

  let in7 =
        [dt 0 Nothing, dt 10 (Just (rA (v2 20 20))), dt 2 Nothing, dt 1 Nothing]
  rs7 <- test in7
  rs7 @?= [a0, rA (v2 20 20), rA (v2 40 40), rA (v2 50 50)]

  let in8 =
        [ dt 0  Nothing
        , dt 10 (Just (rA (v2 20 20)))
        , dt 2  Nothing
        , dt 1  Nothing
        , dt 1  (Just (rA (v2 20 20)))
        ]
  rs8 <- test in8
  rs8 @?= [a0, rA (v2 20 20), rA (v2 40 40), rA (v2 50 50), rA (v2 20 20)]

  -- extrapolate function must not have internal state
  -- e.g when extrapolating A with velocity > 0
  -- then extrapolating A with velocity = 0, should yield the same position

  let rAv0 pos = a0 { posA = pos, velA = v2 0 0 }
  let in9 =
        [ dt 1  (Just (rA (v2 20 20)))
        , dt 4  Nothing
        , dt 1  (Just (rAv0 (v2 100 100)))
        , dt 10 Nothing
        ]
  rs9 <- test in9
  rs9 @?= [rA (v2 20 20), rA (v2 60 60), rAv0 (v2 100 100), rAv0 (v2 100 100)]

