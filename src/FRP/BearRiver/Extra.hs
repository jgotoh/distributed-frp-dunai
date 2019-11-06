module FRP.BearRiver.Extra where

import           Control.Monad.Trans.MSF
import           Control.Monad.Trans.MSF.Except
                                               as MSF
import           Data.Functor.Identity
import           Data.Maybe
import           Data.MonadicStreamFunction.InternalCore
import           FRP.BearRiver

reactimateNet
  :: Monad m
  => m a
  -> (Bool -> m (DTime, Maybe a))
  -> (Bool -> b -> m Bool)
  -> SF Identity a b
  -> m (Maybe c)
  -> (((DTime, a), Maybe c) -> (DTime, a))
  -> (b -> m ())
  -> m ()
reactimateNet senseI sense actuate sf netin combine netout = do
  MSF.reactimateB
    $   (senseSF senseI sense &&& (arrM $ \() -> netin))
    >>> arr combine
    >>> sfIO sf
    >>> (actuateSF actuate)
    &&& (withSideEffect netout)
    >>> arr fst
  return ()

reactimateNet'
  :: Monad m
  => m a
  -> (Bool -> m (DTime, Maybe a))
  -> (Bool -> b -> m Bool)
  -> SF Identity (a,Maybe c) b
  -> m (Maybe c)
  -> (b -> m ())
  -> m ()
reactimateNet' senseI sense actuate sf netin netout = do
  MSF.reactimateB
    $   (senseSF senseI sense &&& (arrM $ \() -> netin))
    >>> arr reorder
    >>> sfIO sf
    >>> (actuateSF actuate)
    &&& (withSideEffect netout)
    >>> arr fst
  return ()

reorder :: ((DTime, a), Maybe c) -> (DTime, (a, Maybe c))
reorder ((t, a), c) = (t, (a, c))

sfIO :: Monad m => MSF (ReaderT r Identity) a b -> MSF m (r, a) b
sfIO sf = morphS (return . runIdentity) (runReaderS sf)

-- Sense, c := GameInput
senseSF
  :: (Monad m, Num a1) => m c -> (Bool -> m (a1, Maybe c)) -> MSF m a (a1, c)
senseSF senseI sense = switch' (senseFirst senseI) (senseRest sense)

senseFirst :: (Monad m, Num a1) => m b -> MSF m a2 ((a1, b), Event b)
senseFirst senseI = constM senseI >>> (arr $ \x -> ((0, x), Event x))

senseRest :: Monad m => (Bool -> m (c, Maybe c')) -> c' -> MSF m a (c, c')
senseRest sense a = constM (sense True) >>> (arr id *** keepLast a)

keepLast :: Monad m => a -> MSF m (Maybe a) a
keepLast a = MSF $ \ma -> let a' = fromMaybe a ma in return (a', keepLast a')

-- Consume/render
actuateSF :: Monad m => (Bool -> a -> m c) -> MSF m a c
actuateSF actuate = arr (\x -> (True, x)) >>> arrM (uncurry actuate)


switch' :: Monad m => MSF m a (b, Event c) -> (c -> MSF m a b) -> MSF m a b
switch' sf' sfC = MSF.switch (sf' >>> second (arr eventToMaybe)) sfC

