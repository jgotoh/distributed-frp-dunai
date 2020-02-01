module Data.MonadicStreamFunction.Extra where

import           Debug.Trace
import           Data.MonadicStreamFunction
                                         hiding ( trace )
import           Data.MonadicStreamFunction.InternalCore

-- | Well-formed looped connection of an output component as a future input.
-- Receives initial input via monadic action
feedbackM :: Monad m => m c -> MSF m (a, c) (b, c) -> MSF m a b
feedbackM act sf = MSF $ \a -> do
  c               <- act
  ((b', c'), sf') <- unMSF sf (a, c)
  return (b', feedback c' sf')

arrTrace :: (Monad m, Show a) => MSF m a a
arrTrace = arr (\x -> trace (show x) x)

traceSF :: (Monad m, Show b) => MSF m a b -> MSF m a b
traceSF sf = sf >>> arrTrace

