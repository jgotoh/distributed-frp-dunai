{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BearRiverTimeWarpTest
  ( bearRiverTimeWarpTests
  )
where

import           GHC.Exts
import           Prelude                 hiding ( pi
                                                , concat
                                                )
import           FRP.BearRiver.Network.TimeWarp
import           Control.Exception
import           Data.MessageBuffer
import           Numeric.Natural
import           FRP.BearRiver
import           Network.Common
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.MonadicStreamFunction.Network.TimeWarp

bearRiverTimeWarpTests :: TestTree
bearRiverTimeWarpTests = tests

tests :: TestTree
tests = testGroup
  "BearRiverTimeWarpTests"
  [ testCase "testing rollbacks" testPerformRollback
  , testCase "Adding single new input to list of processed inputs"
             testAddProcessed
  , testCase
    "Updating list of processed inputs after rollbacks // Merge new updates into a previous list of updates"
    testProcessedAfterRollback
  , testCase "Conversion of list of ProcessedInputs to MSF inputs"
             testRollbackInputs
  , testCase "Test steps in SFs with and without rollbacks" testTimeWarpStep
  ]

data TestMessage = TM FrameNr Char
  deriving (Eq, Show)

instance HasFrameAssociation TestMessage where
  getFrame (TM n _) = n

instance Ord (TestMessage) where
  (<=) (TM n _) (TM n' _) = n <= n'

data SFOut = SFOut Integer [TestMessage]
  deriving (Show, Eq)

-- Returns the current frameNr and its input concatenated with previous inputs to enable testing rollbacks
concatSFWarp :: MSF IO (Natural, (Integer, [TestMessage])) (Integer, [SFOut])
concatSFWarp = rollbackMSF 3 $ concatSF

concatSF :: Monad m => MSF m (Integer, [TestMessage]) (Integer, [SFOut])
concatSF = count &&& concatInput

concatInput :: Monad m => MSF m (Integer, [TestMessage]) [SFOut]
concatInput =
  feedback [] $ (arr $ \((x, q), p) -> p ++ [SFOut x q]) >>> arr dup

-- tests whether stepSF correctly rolls back on new messages for older frames.
testTimeWarpStep :: Assertion
testTimeWarpStep = do
  let input frameNr c x = (frameNr, (c, fromList x))
      sf = stepSF concatSFWarp 5

  -- Test one sf application / One frame

  -- no messages -> step normally
  let in1 = [input 1 10 []]

  -- embed sf returns a list where each element is the output that results of sf application
  rs1 <- embed sf in1
  rs1 @?= [(1, [SFOut 10 []])]

  -- no processed input for a previous frame 0 -> rollback fails
  let in1a = [input 1 10 [TM 0 'a']]
  assertThrows $ embed sf in1a

  -- message for a future frame -> fail
  let in1b = [input 1 10 [TM 0 'a']]
  assertThrows $ embed sf in1b

  -- message for current frame -> step normally with message
  let in1c = [input 1 10 [TM 1 'a']]
  rs1c <- embed sf in1c
  rs1c @?= [(1, [SFOut 10 [TM 1 'a']])]

  -- Two SF applications / Two frames

  -- normal step
  let in2 = [input 1 10 [], input 2 11 []]
  rs2 <- embed sf in2
  rs2 @?= [(1, [SFOut 10 []]), (2, [SFOut 10 [], SFOut 11 []])]

  -- input for frame 0 -> fail
  let in2a = [input 1 10 [], input 2 11 [TM 0 'a']]
  assertThrows $ embed sf in2a

  -- input for frame 3 at frame 2 -> throws error
  let in2b = [input 1 10 [], input 2 11 [TM 3 'a']]
  assertThrows $ embed sf in2b

  -- rollback to frame 1 with a new message
  let in2c = [input 1 10 [], input 2 11 [TM 1 'b']]
  rs2c <- embed sf in2c
  rs2c @?= [(1, [SFOut 10 []]), (2, [SFOut 10 [TM 1 'b'], SFOut 11 []])]

  -- rollback, also new message for current frame 2
  let in2d = [input 1 10 [], input 2 11 [TM 1 'b', TM 2 'c']]
  rs2d <- embed sf in2d
  rs2d @?= [(1, [SFOut 10 []]), (2, [SFOut 10 [TM 1 'b'], SFOut 11 [TM 2 'c']])]

  -- multiple messages for current frame, unsorted
  let in2e = [input 1 10 [], input 2 11 [TM 2 'a', TM 1 'b', TM 2 'c']]
  rs2e <- embed sf in2e
  rs2e
    @?= [ (1, [SFOut 10 []])
        , (2, [SFOut 10 [TM 1 'b'], SFOut 11 [TM 2 'a', TM 2 'c']])
        ]

  -- multiple messages for last frame and current frame, unsorted
  let in2f =
        [input 1 10 [], input 2 11 [TM 2 'a', TM 1 'b', TM 2 'c', TM 1 'd']]
  rs2f <- embed sf in2f
  rs2f
    @?= [ (1, [SFOut 10 []])
        , (2, [SFOut 10 [TM 1 'b', TM 1 'd'], SFOut 11 [TM 2 'a', TM 2 'c']])
        ]

  -- Three applications / Three frames

  -- three normal applications
  let in3 = [input 1 10 [], input 2 11 [], input 3 12 []]
  rs3 <- embed sf in3
  rs3
    @?= [ (1, [SFOut 10 []])
        , (2, [SFOut 10 [], SFOut 11 []])
        , (3, [SFOut 10 [], SFOut 11 [], SFOut 12 []])
        ]

  -- one application, then rollback, then normal
  let in3a = [input 1 10 [], input 2 11 [TM 1 'b'], input 3 12 []]
  rs3a <- embed sf in3a
  rs3a
    @?= [ (1, [SFOut 10 []])
        , (2, [SFOut 10 [TM 1 'b'], SFOut 11 []])
        , (3, [SFOut 10 [TM 1 'b'], SFOut 11 [], SFOut 12 []])
        ]
  -- one application, then rollback, then rollback
  let in3b =
        [ input 1 10 []
        , input 2 11 [TM 1 'b']
        , input 3 12 [TM 1 'c', TM 2 'd', TM 3 'e', TM 3 'f']
        ]
  rs3b <- embed sf in3b
  rs3b
    @?= [ (1, [SFOut 10 []])
        , (2, [SFOut 10 [TM 1 'b'], SFOut 11 []])
        , ( 3
          , [ SFOut 10 [TM 1 'b', TM 1 'c']
            , SFOut 11 [TM 2 'd']
            , SFOut 12 [TM 3 'e', TM 3 'f']
            ]
          )
        ]

-- Necessary for idSFWarp to be usable in timeWarpStep
instance HasFrameAssociation Integer where
  getFrame = fromIntegral

idSFWarp
  :: Monad m => MSF m (Natural, (Char, [Integer])) (Integer, (Char, [Integer]))
idSFWarp = rollbackMSF 10 $ idSF

-- sf that just returns the current frameNr/ number of iteration and its input
idSF :: Monad m => MSF m (a, [msg]) (Integer, (a, [msg]))
idSF = count &&& arr id

-- Tests whether performRollback and processInputs correctly apply MSFs.
testPerformRollback :: Assertion
testPerformRollback = do
  let input n c x = (n, (c, x))
      in0 = [input 0 'a' [], input 0 'b' [], input 0 'c' []]

  rs0  <- processInputs' idSFWarp in0
      -- verify that performRollback returns the same last element as processInputs'
  rs0' <- performRollback idSFWarp in0

  fst <$> rs0 @?= [(1, ('a', [])), (2, ('b', [])), (3, ('c', []))]
  fst rs0' @?= (3, ('c', []))

  -- warp back to first iteration on second iteration
  let in1 = [input 0 'a' [], input 1 'b' [], input 0 'c' []]
  rs1  <- processInputs' idSFWarp in1
  rs1' <- performRollback idSFWarp in1

  fst <$> rs1 @?= [(1, ('a', [])), (1, ('b', [])), (2, ('c', []))]
  fst rs1' @?= (2, ('c', []))

  -- performRollback on empty inputs will throw an error
  let in2 = []
  rs2 <- processInputs' idSFWarp in2

  fst <$> rs2 @?= []
  assertThrows $ do
    x <- performRollback idSFWarp in2
    print $ fst x


buf :: (Integer, [Char]) -> (Integer, MessageBuffer Char)
buf (n, cs) = (n, fromList cs)

-- Eq instance of ProcessedInput only compares frame numbers.
-- When testing however, we need to compare all contents.
newtype DeepEq a msg = DeepEq (ProcessedInput a msg)
  deriving Show

instance (Eq a, Eq msg) => Eq (DeepEq a msg) where
  (==) (DeepEq (ProcessedInput x)) (DeepEq (ProcessedInput y)) = x == y

testAddProcessed :: Assertion
testAddProcessed = do
  let pi n a ms = ProcessedInput (n, (a, ms))

  let b0  = fromList [] :: MessageBuffer (ProcessedInput Integer Char)
      rs0 = addProcessed b0 2 $ buf (1, ['a'])

  -- add to empty list
  rs0 @?= fromList [pi 2 1 ['a']]

  -- add before an element
  let rs1 = addProcessed rs0 0 $ buf (2, ['b'])

  rs1 @?= fromList [pi 0 2 ['b'], pi 2 1 ['a']]

  -- add after an element
  let rs2 = addProcessed rs1 3 $ buf (3, ['c'])

  rs2 @?= fromList [pi 0 2 ['b'], pi 2 1 ['a'], pi 3 3 ['c']]

  -- add when element with same index already exists
  let rs3 = addProcessed rs2 2 $ buf (2, ['d'])

  DeepEq
    <$> (toList rs3)
    @?= DeepEq
    <$> [pi 0 2 ['b'], pi 2 1 ['a'], pi 2 2 ['d'], pi 3 3 ['c']]


type TestBuffer = MessageBuffer (ProcessedInput Integer TestMessage)

testProcessedAfterRollback :: Assertion
testProcessedAfterRollback = do

  let b0  = fromList [] :: TestBuffer -- buffer at previous frame
      f0  = 1  -- frame
      a0  = 10 -- a at frame
      ms0 = fromList [TM 0 'a'] -- new messages, will be discarded, because no matching frame exists
      rs0 = processedAfterRollback b0 f0 a0 ms0

  isEq rs0 [(1, (10, []))]

  let b1  = rs0
      f1  = 2
      a1  = 11
      ms1 = fromList []
      rs1 = processedAfterRollback b1 f1 a1 ms1

  isEq rs1 [(1, (10, [])), (2, (11, []))]

  let b2  = rs1
      f2  = 3
      a2  = 12
      ms2 = fromList [TM 1 'a'] -- add a single message to a previous frame
      rs2 = processedAfterRollback b2 f2 a2 ms2

  isEq rs2 [(1, (10, [TM 1 'a'])), (2, (11, [])), (3, (12, []))]

  let b3  = rs2
      f3  = 4
      a3  = 13
      ms3 = fromList [TM 1 'b', TM 1 'c', TM 2 'd']
      rs3 = processedAfterRollback b3 f3 a3 ms3

  isEq
    rs3
    [ (1, (10, [TM 1 'a', TM 1 'b', TM 1 'c']))
    , (2, (11, [TM 2 'd']))
    , (3, (12, []))
    , (4, (13, []))
    ]

  let b4  = rs3
      f4  = 5
      a4  = 14
      ms4 = fromList [TM 5 'e'] -- input for current frame
      rs4 = processedAfterRollback b4 f4 a4 ms4

  isEq
    rs4
    [ (1, (10, [TM 1 'a', TM 1 'b', TM 1 'c']))
    , (2, (11, [TM 2 'd']))
    , (3, (12, []))
    , (4, (13, []))
    , (5, (14, [TM 5 'e']))
    ]

  let b5  = rs4
      f5  = 6
      a5  = 15
      ms5 = fromList [TM 6 'f', TM 6 'g']
      rs5 = processedAfterRollback b5 f5 a5 ms5

  isEq
    rs5
    [ (1, (10, [TM 1 'a', TM 1 'b', TM 1 'c']))
    , (2, (11, [TM 2 'd']))
    , (3, (12, []))
    , (4, (13, []))
    , (5, (14, [TM 5 'e']))
    , (6, (15, [TM 6 'f', TM 6 'g']))
    ]

-- Checks if buf is equivalent to xs. Maps xs to ProcessedInputs, uses DeepEq for equality.
isEq
  :: MessageBuffer (ProcessedInput Integer TestMessage)
  -> [(FrameNr, (Integer, [TestMessage]))]
  -> Assertion
isEq bf xs = DeepEq <$> toList bf @?= deq <$> xs
  where deq = DeepEq . ProcessedInput

-- Tests if rollbackInputs selects the correct inputs used when rolling back from frame t to t0.
-- Also checks if the correct delta between t0 and t is set in the head, which initiates the rollback of a signal function.
testRollbackInputs :: Assertion
testRollbackInputs = do
  let pi n a ms = ProcessedInput (n, (a, ms))

  let b0  = fromList [] :: MessageBuffer (ProcessedInput Integer Char)
      rs0 = rollbackInputs b0 0 10
  rs0 @?= []

  -- none are dropped
  let b1  = fromList [pi 0 3 ['a'], pi 1 2 ['b'], pi 2 1 ['c']]
      rs1 = rollbackInputs b1 0 2
  rs1 @?= [(2, (3, ['a'])), (0, (2, ['b'])), (0, (1, ['c']))]

  -- one is dropped
  let rs2 = rollbackInputs b1 1 2
  rs2 @?= [(1, (2, ['b'])), (0, (1, ['c']))]

  -- all but one are dropped
  let rs3 = rollbackInputs b1 2 2
  rs3 @?= [(0, (1, ['c']))]

  -- exception, rolling into the future
  assertThrows $ do
    let rs4 = rollbackInputs b1 2 0
    print rs4

  return ()

assertThrows :: IO a -> IO ()
assertThrows f =
  catch (f >> return False) (\(_ :: SomeException) -> return True)
    >>= \r -> r @?= True

