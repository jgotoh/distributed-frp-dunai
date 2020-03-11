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

-- delayed switching
-- Taken from https://hackage.haskell.org/package/bearriver-0.13.1.1/docs/src/FRP.BearRiver.html#dSwitch with type generalized to any MSF
dSwitch' :: Monad m => MSF m a (b, Maybe c) -> (c -> MSF m a b) -> MSF m a b
dSwitch' sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (b, Just c) -> do
      return (b, sfC c)
    (b, Nothing) -> return (b, dSwitch' ct sfC)

-- replace first input to an MSF
-- Taken from https://hackage.haskell.org/package/bearriver-0.13.1.1/docs/src/FRP.BearRiver.html#dSwitch with type generalized to any MSF
replaceOnce' :: Monad m => a -> MSF m a a
replaceOnce' a = dSwitch' (arr $ const (a, Just ())) (const $ arr id)

-- Returns x on first application, after that sf is used.
replaceOnceOut :: Monad m => b -> MSF m a b -> MSF m a b
replaceOnceOut x sf = dSwitch' (arr (\_ -> x) &&& arr (\_ -> Just ())) (\_ -> sf)
