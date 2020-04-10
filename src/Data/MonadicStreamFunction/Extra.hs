-- | Additional functions related to MSFs.

module Data.MonadicStreamFunction.Extra where

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

-- | Delayed switching
-- Taken from https://hackage.haskell.org/package/bearriver-0.13.1.1/docs/src/FRP.BearRiver.html#dSwitch with type generalized to any MSF
dSwitch' :: Monad m => MSF m a (b, Maybe c) -> (c -> MSF m a b) -> MSF m a b
dSwitch' sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (b, Just c ) -> return (b, sfC c)
    (b, Nothing) -> return (b, dSwitch' ct sfC)

-- | Replace first input to an MSF.
-- Taken from https://hackage.haskell.org/package/bearriver-0.13.1.1/docs/src/FRP.BearRiver.html#dSwitch with type generalized to any MSF
replaceOnce' :: Monad m => a -> MSF m a a
replaceOnce' a = dSwitch' (arr $ const (a, Just ())) (const $ arr id)

-- | Return 'b' on first application, after that the MSF is used to produce new values.
replaceOnceOut :: Monad m => b -> MSF m a b -> MSF m a b
replaceOnceOut x sf =
  dSwitch' (arr (const x) &&& arr (\_ -> Just ())) (const sf)

