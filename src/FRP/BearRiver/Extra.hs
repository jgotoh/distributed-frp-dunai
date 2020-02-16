{-# LANGUAGE RankNTypes #-}
module FRP.BearRiver.Extra
  ( reactimateServer
  , reactimateClient
  , edgeJust
  )
where

import           Control.Monad.Trans.MSF
import           Control.Monad.Trans.MSF.Except
                                               as MSF
import           Data.Functor.Identity
import           Data.Maybe
import           Data.MonadicStreamFunction.InternalCore
import           FRP.BearRiver hiding (edgeJust)

reactimateServer
  :: Monad m
  => m a
  -> (Bool -> m (DTime, Maybe a)) -- sense
  -> (Bool -> b -> m Bool) -- actuate
  -> SF Identity (a, Maybe netin) b -- sf
  -> m (Maybe netin) -- action to get an UpdatePacket
  -> (b -> m ()) -- action that writes a CommandPacket
  -> m ()
reactimateServer senseI sense actuate sf netin netout = do
  MSF.reactimateB
    $   (senseSF senseI sense &&& (arrM $ \() -> netin))
    >>> arr reorder
    >>> sfIO sf
    >>> (actuateSF actuate)
    &&& (withSideEffect netout)
    >>> arr fst
  return ()

reactimateClient
  :: Monad m
  => m a
  -> (Bool -> m (DTime, Maybe a)) -- sense
  -> (Bool -> b -> m Bool) -- actuate
  -> SF Identity (a, Maybe netin) b -- sf
  -> m (Maybe netin) -- action to get an UpdatePacket
  -> ((DTime, Maybe a) -> m ()) -- action that writes a CommandPacket
  -> m ()
reactimateClient senseI sense actuate sf netin netout = do
  MSF.reactimateB
    $   (senseSFClient senseI sense netout &&& (arrM $ \() -> netin))
    >>> arr reorder
    >>> sfIO sf
    >>> (actuateSF actuate)
  return ()

-- Sense functions

-- sense input and send CommandPackets
senseSFClient
  :: Monad m
  => m a -- initial sense
  -> (Bool -> m (DTime, Maybe a)) -- sense
  -> ((DTime, Maybe a) -> m ()) -- netout, send if a is Just value
  -> MSF m () (DTime, a) -- returns last a if sense's a is Nothing
senseSFClient senseI sense out =
  switch' (senseFirstClient senseI out) (senseRestClient sense out)

senseFirstClient
  :: Monad m
  => m a
  -> ((DTime, Maybe a) -> m ())
  -> MSF m () ((DTime, a), Event a)
senseFirstClient senseI out =
  constM senseI
    >>> arr (\x -> (0, x))
    >>> (arr $ \x -> (x, Event $ snd x))
    &&& (second (arr Just) >>> withSideEffect out)
    >>> arr fst

senseRestClient
  :: Monad m
  => (Bool -> m (DTime, Maybe a))
  -> ((DTime, Maybe a) -> m ())
  -> a
  -> MSF m () (DTime, a)
senseRestClient sense out a =
  constM (sense True)
    >>> ((arr id *** keepLast a) &&& withSideEffect out)
    >>> arr fst

reorder :: ((DTime, a), Maybe c) -> (DTime, (a, Maybe c))
reorder ((t, a), c) = (t, (a, c))

sfIO :: Monad m => MSF (ReaderT r Identity) a b -> MSF m (r, a) b
sfIO sf = morphS (return . runIdentity) (runReaderS sf)

-- Sense, c := GameInput, if sense returns Nothing -> senseSF returns the last value
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

edgeJust :: Monad m => SF m (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
 where
  isJustEdge Nothing  Nothing     = Nothing
  isJustEdge Nothing  ma@(Just _) = ma
  isJustEdge (Just _) (   Just _) = Nothing
  isJustEdge (Just _) Nothing     = Nothing

