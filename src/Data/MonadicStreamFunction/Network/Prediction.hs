-- | MSFs related to prediction of signals. MSFs mapping discrete signals to continuous signals.
-- Dead Reckoning works by extrapolation of the last defined values.
-- Client Side Prediction, see 'predict', works by simulating last defined values.

{-# LANGUAGE RankNTypes #-}

module Data.MonadicStreamFunction.Network.Prediction
  ( drm
  , predict
  )
where

import           Data.MonadicStreamFunction
                                         hiding ( trace )
import           Data.MonadicStreamFunction.Extra
import           Data.VectorSpace
import           Control.Monad.Trans.MSF.Except

-- | Client Side Prediction, either returns a 'b' if input signal 'Maybe b' is defined, or uses 'MSF m a b' if it is not defined. Switches into a new simulating sf 'MSF m a b', if input signal is defined.
predict
  :: Monad m
  => MSF m a b -- ^ initial simulating sf
  -> (b -> MSF m a b) -- ^ get new updated simulating sf on defined values
  -> MSF m (a, Maybe b) b
predict sf sfC = switch
  (simulate sf)
  (\b ->
    second (replaceOnce' Nothing) >>> replaceOnceOut b (predict (sfC b) sfC)
  )

simulate :: Monad m => MSF m a b -> MSF m (a, Maybe b) (b, Maybe b)
simulate = first

-- | Generic Dead Reckoning, extrapolate last defined values of type 'b'.
-- values of type b need a velocity, position. Positions are extrapolated, so the function needs a way to construct new values from updated positions. Integration is done by MSF m v v.
drm
  :: (VectorSpace v a, Monad m)
  => MSF m v v -- ^ integrating MSF
  -> b -- ^ initial b to extrapolate
  -> (b -> v) -- ^ velocity vector
  -> (b -> v) -- ^ position vector
  -> (b -> v -> b) -- ^ contruct new value from extrapolated position
  -> MSF m (Maybe b) b
drm integrate b0 vel pos new = switch
  (extrapolateSF integrate b0 vel pos new)
  (\b0' -> replaceOnce' Nothing
    >>> replaceOnceOut b0' (drm integrate b0' vel pos new)
  )
  -- first input needs to be replaced by Nothing, otherwise infinite loop
  -- then ensure that new Just values are not extrapolated immediately by first returning b0', then switch into extrapolating arrow

extrapolateSF
  :: (VectorSpace v a, Monad m)
  => MSF m v v
  -> b
  -> (b -> v)
  -> (b -> v)
  -> (b -> v -> b)
  -> MSF m (Maybe b) (b, Maybe b)
extrapolateSF int b0 vel pos new = proc mb -> do
  b' <- extrapolate vel pos new int -< b0
  returnA -< (b', mb)

extrapolate
  :: (Monad m, VectorSpace v a)
  => (b -> v)
  -> (b -> v)
  -> (b -> v -> b)
  -> MSF m v v
  -> MSF m b b
extrapolate vec pos new integrate = proc b -> do
  dp   <- integrate -< vec b
  pos' <- arr (uncurry (^+^)) -< (pos b, dp)
  returnA -< new b pos'

