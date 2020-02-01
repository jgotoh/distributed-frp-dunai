{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TimeWarpTest
  ( timeWarpTests
  )
where

import           Control.Exception
import           Data.MonadicStreamFunction
                                         hiding ( trace )
import           Numeric.Natural
import           Data.MonadicStreamFunction.InternalCore
import           Data.MonadicStreamFunction.TimeWarp
import           Control.Monad.Trans.MSF
import           Test.Tasty
import           Test.Tasty.HUnit


timeWarpTests :: TestTree
timeWarpTests = tests

tests :: TestTree
tests = testGroup
  "TimeWarpTests"
  [ testCase "embed bouncingBall"        testBouncingBall
  , testCase "embed toWarpSF"            testToWarpSF
  , testCase "test consCap"              testConsCap
  , testCase "test selectSF"             testSelectSF
  , testCase "embed warpSF bouncingBall" testWarpSF
  , testCase "embed warpSF count"        testWarpSFCount
  ]

testBouncingBall :: Assertion
testBouncingBall = do
  -- a ball that bounces between 1 and 3
  ys <- embed (bouncingBall 0 1) (replicate 10 ())
  ys @?= [1, 2, 3, 3, 2, 1, 1, 2, 3, 3]
  print ys

testConsCap :: Assertion
testConsCap = do
  consCap 0 1 [] @?= []
  consCap 1 3 [] @?= [3]
  consCap 2 3 [4] @?= [3, 4]
  consCap 2 3 [2, 4] @?= [3, 2]
  consCap 5 1 [2, 3, 4, 5, 6] @?= [1, 2, 3, 4, 5]

testSelectSF :: Assertion
testSelectSF = do
  -- selects (arr $ \() -> 10)
  (sf0, [sf0']) <- selectSF 0 (arr $ \() -> 10) [(arr $ \() -> 11)]
  (x0 , _     ) <- unMSF sf0 ()
  x0 @?= (10 :: Integer)

  (x0', _) <- unMSF sf0' ()
  x0' @?= (11 :: Integer)

  assertThrows $ selectSF 1 (arr $ \() -> 10 :: Integer) []

  -- selects (arr $ \() -> 11)
  (sf1, []) <- selectSF 1 (arr $ \() -> 10) [(arr $ \() -> 11)]
  (x1 , _ ) <- unMSF sf1 ()
  x1 @?= (11 :: Integer)

  -- selects (arr $ \() -> 12), rest of the list is thrown away
  (sf2, []) <- selectSF 2
                        (arr $ \() -> 10)
                        [(arr $ \() -> 11), (arr $ \() -> 12)]
  (x2, _) <- unMSF sf2 ()
  x2 @?= (12 :: Integer)

  -- selects (arr $ \() -> 12)
  (sf3, [sf3']) <- selectSF
    2
    (arr $ \() -> 10)
    [(arr $ \() -> 11), (arr $ \() -> 12), (arr $ \() -> 13)]
  (x3, _) <- unMSF sf3 ()
  x3 @?= (12 :: Integer)
  (x3', _) <- unMSF sf3' ()
  x3' @?= (13 :: Integer)

-- Tests toWarpSF with manually created input continuations to choose from
testToWarpSF :: Assertion
testToWarpSF = do
  let sf =
        toWarpSF 10 $ bouncingBall 0 1 :: MSF
            IO
            ((Natural, ()), [MSF IO () Ball])
            (Ball, [MSF IO () Ball])

  let sfInput x cs = ((x, ()), cs)

  -- apply 1 time
  rs0 <- unMSF sf $ sfInput 0 []
  (fst . fst) rs0 @?= 1

  rs1 <- embed sf [sfInput 0 []]
  fst <$> rs1 @?= [1]

  -- replicating sfInput 0 10 times, should yield the same values as bouncingBall.
  rs2 <- embed sf $ replicate 10 $ sfInput 0 []
  -- print $ fst <$> rs2
  fst <$> rs2 @?= [1, 2, 3, 3, 2, 1, 1, 2, 3, 3]

  -- warp, return 10
  rs3 <- embed sf $ [sfInput 1 [arr $ \() -> 10]]
  fst <$> rs3 @?= [10]

  -- warp, return 11
  rs4 <- embed sf $ [sfInput 2 [arr $ \() -> 10, arr $ \() -> 11]]
  fst <$> rs4 @?= [11]

  -- apply normally twice, then warp and return 10
  rs5 <- embed sf $ [sfInput 0 [], sfInput 0 [], sfInput 1 [arr $ \() -> 10]]
  fst <$> rs5 @?= [1, 2, 10]

  assertThrows (embed sf $ [sfInput 1 []])

-- Tests warpSF using count SF
testWarpSFCount :: Assertion
testWarpSFCount = do
  let sf = warpSF 10 $ count :: MSF IO (Natural, ()) Integer
      sfInput x = ((x, ()))

  rs0 <- embed sf $ replicate 5 $ sfInput 0
  rs0 @?= [1 .. 5]

  rs1 <- embed sf [sfInput 0, sfInput 0, sfInput 2]
  rs1 @?= [1, 2, 1]

  rs2 <- embed sf [sfInput 0, sfInput 0, sfInput 2, sfInput 0, sfInput 0]
  rs2 @?= [1, 2, 1, 2, 3]

  rs3 <- embed sf [sfInput 0, sfInput 0, sfInput 2, sfInput 0, sfInput 2]
  rs3 @?= [1, 2, 1, 2, 1]

  -- sequential warps prevent sf from progressing
  rs4 <- embed sf [sfInput 0, sfInput 0, sfInput 1, sfInput 1, sfInput 1]
  rs4 @?= [1, 2, 2, 2, 2]


-- Tests warpSF using bouncingBall
testWarpSF :: Assertion
testWarpSF = do
  let sf = warpSF 10 $ bouncingBall 0 1
      sfInput x = ((x, ()))

  -- Never warp, always select next continuation
  -- Should yield the same values as bouncingBall
  rs0 <- embed sf $ replicate 10 $ sfInput 0
  -- print $ fst <$> rs2
  rs0 @?= [1, 2, 3, 3, 2, 1, 1, 2, 3, 3]

  assertThrows (embed sf [sfInput 1])

  -- apply normally, then warp to first state
  rs1 <- embed sf [sfInput 0, sfInput 1]
  rs1 @?= [1, 1]

  rs2 <- embed sf [sfInput 0, sfInput 1, sfInput 0, sfInput 0]
  rs2 @?= [1, 1, 2, 3]

  -- at sfInput 1: ball moves from 3 to 2 again
  rs3 <- embed
    sf
    [sfInput 0, sfInput 0, sfInput 0, sfInput 0, sfInput 0, sfInput 1]
  rs3 @?= [1, 2, 3, 3, 2, 2]

  -- at sfInput 2: ball moves from 3 to 3
  rs4 <- embed
    sf
    [sfInput 0, sfInput 0, sfInput 0, sfInput 0, sfInput 0, sfInput 2]
  rs4 @?= [1, 2, 3, 3, 2, 3]

  -- at sfInput 3: ball moves from 2 to 3, before switch to left movement occured
  rs5 <- embed
    sf
    [sfInput 0, sfInput 0, sfInput 0, sfInput 0, sfInput 0, sfInput 3]
  rs5 @?= [1, 2, 3, 3, 2, 3]

  -- sfInput 5: ball warps to original position 0 and moves to the right again
  rs6 <- embed
    sf
    [ sfInput 0 -- 5: 0 -> 1
    , sfInput 0 -- 4: 1 -> 2
    , sfInput 0 -- 3: 2 -> 3
    , sfInput 0 -- 2: 3 -> 3
    , sfInput 0 -- 1: 3 -> 2
    , sfInput 5 -- warp back to 0 -> 1
    , sfInput 0 -- 0: 1 -> 2
    , sfInput 0 -- 0: 2 -> 3
    , sfInput 0 -- 0: 3 -> 3
    ]
  rs6 @?= [1, 2, 3, 3, 2, 1, 2, 3, 3]

  -- Warp back twice
  rs7 <- embed
    sf
    [ sfInput 0 -- 5: 0 -> 1
    , sfInput 0 -- 4: 1 -> 2
    , sfInput 0 -- 3: 2 -> 3
    , sfInput 0 -- 2: 3 -> 3
    , sfInput 0 -- 1: 3 -> 2
    , sfInput 5 -- warp back to 0 -> 1
    , sfInput 0 -- 0: 1 -> 2
    , sfInput 2 -- warp back to 0 -> 1
    , sfInput 0 -- 0: 1 -> 2
    ]
  rs7 @?= [1, 2, 3, 3, 2, 1, 2, 1, 2]

type Ball = Int
type Direction = Int

leftPlayerPos :: Ball
leftPlayerPos = 0

rightPlayerPos :: Ball
rightPlayerPos = 4

-- move in direction d0 until ball hits either leftPlayerPos or rightPlayerPos, then bounces back
-- event is created when ball is on playerPos, it will instantly get corrected/ switched into, so the ball never actually reaches a position of a player
bouncingBall :: Monad m => Ball -> Direction -> MSF m () Ball
bouncingBall p0 d0 = switch (movingBall p0 d0 >>> (arr fst &&& hitFloorSF))
                            (\(p, d) -> bouncingBall p (-d))

movingBall :: Monad m => Ball -> Direction -> MSF m () (Ball, Direction)
movingBall p0 d0 = count >>> arr (\n -> (p0 + (d0 * n), d0))

hitFloorSF :: Monad m => MSF m (Ball, Direction) (Maybe (Ball, Direction))
hitFloorSF = arr hitFloor

hitFloor :: (Ball, Direction) -> Maybe (Ball, Direction)
hitFloor (b, d) | d < 0 && b == leftPlayerPos   = Just (b, d)
                | d >= 0 && b == rightPlayerPos = Just (b, d)
                | otherwise                     = Nothing

assertThrows :: IO a -> IO ()
assertThrows f =
  catch (f >> return False) (\(_ :: SomeException) -> return True)
    >>= \r -> r @?= True

