{-# LANGUAGE RankNTypes #-}
module FRP.BearRiver.Extra
  ( reactimateServer
  , reactimateClient
  , edgeJust
  , countAt
  , frameNrSF
  , HasFrameAssociation(..)
  , FrameNr
  , embedSF
  )
where

import           Control.Monad
import           Network.Common
import           Control.Monad.Trans.MSF
import           Control.Monad.Trans.MSF.Except
                                               as MSF
import           Data.Functor.Identity
import           Data.Maybe
import           Data.MonadicStreamFunction.InternalCore
import           FRP.BearRiver           hiding ( edgeJust )

reactimateServer
  :: Monad m
  => m a
  -> (Bool -> m (DTime, Maybe a)) -- sense
  -> (Bool -> b -> m Bool) -- actuate
  -> SF Identity (a, [netin]) b -- sf
  -> m [netin] -- action to get an UpdatePacket
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
  :: (HasFrameAssociation netin, Monad m)
  => m a
  -> (Bool -> m (DTime, Maybe a)) -- sense
  -> (Bool -> b -> m Bool) -- actuate
  -> SF Identity (a, Maybe netin) b -- sf
  -> m (Maybe netin) -- action to get an UpdatePacket
  -> (FrameNr -> (DTime, a) -> m ()) -- action that writes a CommandPacket
  -> m ()
reactimateClient senseI sense actuate sf netin netout = do
  MSF.reactimateB
    $   (senseSF senseI sense &&& (arrM $ \() -> netin))
    >>> arr reorder
    >>> sfIO sf
    &&& sendCommand netout
    >>> arr fst
    >>> (actuateSF actuate)
  return ()

-- Get the current FrameNr. Starts at x0, increments with each iteration.
-- Current frameNr will be updated when netin contains a value.
frameNrSF
  :: (HasFrameAssociation netin, Monad m)
  => FrameNr
  -> MSF m (Maybe netin) FrameNr
frameNrSF x0 =
  switch' (countAndUpdate x0) (\x1 -> replaceOnce' Nothing >>> frameNrSF x1)

countAndUpdate
  :: (HasFrameAssociation netin, Monad m)
  => FrameNr
  -> MSF m (Maybe netin) (FrameNr, Event FrameNr)
countAndUpdate x0 = countAt x0 &&& arr ((fmap getFrame) . maybeToEvent)

countAt :: (Monad m, Num n) => n -> MSF m a n
countAt x0 = count >>> iPre 0 >>> arr ((+) x0)

-- Sense functions

-- -- Determine current FrameNr and send Commands
sendCommand
  :: (HasFrameAssociation netin, Monad m)
  => (FrameNr -> (DTime, a) -> m ())
  -> MSF m (DTime, (a, Maybe netin)) ()
sendCommand netout = arr id &&& (arr (snd . snd) >>> frameNrSF 0) >>> arrM
  (\((dt, (a, _)), frame) -> netout frame (dt, a))

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

reorder :: ((DTime, a), c) -> (DTime, (a, c))
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

-- Taken from https://hackage.haskell.org/package/bearriver-0.13.1.1/docs/src/FRP.BearRiver.html#dSwitch with type generalized to any MSF
dSwitch' :: Monad m => MSF m a (b, Event c) -> (c -> MSF m a b) -> MSF m a b
dSwitch' sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (b, Event c) -> do
      (_, ct') <- (unMSF (sfC c) a)
      return (b, ct')
    (b, NoEvent) -> return (b, dSwitch' ct sfC)

-- Taken from https://hackage.haskell.org/package/bearriver-0.13.1.1/docs/src/FRP.BearRiver.html#dSwitch with type generalized to any MSF
replaceOnce' :: Monad m => a -> MSF m a a
replaceOnce' a = dSwitch' (arr $ const (a, Event ())) (const $ arr id)

edgeJust :: Monad m => SF m (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
 where
  isJustEdge Nothing  Nothing     = Nothing
  isJustEdge Nothing  ma@(Just _) = ma
  isJustEdge (Just _) (   Just _) = Nothing
  isJustEdge (Just _) Nothing     = Nothing

-- Apply an SF to a list of inputs and delta time values.
embedSF :: Monad m => SF m a b -> [(DTime, a)] -> m [b]
embedSF _  []              = return []
embedSF sf ((dt', a) : as) = do
  (b, sf') <- runReaderT (unMSF sf a) dt'
  bs       <- embedSF sf' as
  return (b : bs)

