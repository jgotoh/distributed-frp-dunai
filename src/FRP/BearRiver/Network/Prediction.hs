-- | SFs related to prediction of signals. SFs mapping discrete signals to continuous signals.
-- Dead Reckoning works by extrapolation of the last defined values.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module FRP.BearRiver.Network.Prediction
  ( drmZero
  , drmFirst
  , drmSecond
  , predict
  , HasPosition(..)
  , HasVelocity(..)
  , HasAcceleration(..)
  )
where

import           Data.VectorSpace
import           Data.MonadicStreamFunction.Network.Prediction
import           FRP.BearRiver

-- | Types that have a position vector.
-- Modules containing instances may need 'FlexibleInstances' and 'MultiParamTypeClasses' language pragmas
class HasPosition a p | a -> p where
  getPosition :: a -> p

-- | Types that have a velocity vector.
class HasVelocity a v | a -> v where
  getVelocity :: a -> v

-- | Types that have an acceleration vector.
class HasAcceleration a v | a -> v where
  getAcceleration :: a -> v

-- | Perform second-order Dead Reckoning by double integration of acceleration.
drmSecond
  :: (Show v, Monad m, VectorSpace v s, HasPosition a v, HasAcceleration a v)
  => a -- ^ initial a to extrapolate
  -> (a -> v -> a) -- ^ function to construct new values from a new position
  -> SF m (Maybe a) a
drmSecond a0 new =
  drm (integral >>> integral) a0 getAcceleration getPosition new

-- | Perform first-order Dead Reckoning by integration of velocity.
drmFirst
  :: (Show v, Monad m, VectorSpace v s, HasPosition a v, HasVelocity a v)
  => a -- ^ initial a to extrapolate
  -> (a -> v -> a) -- ^ function to construct new values from a new position
  -> SF m (Maybe a) a
drmFirst a0 new = drm integral a0 getVelocity getPosition new

-- | Perform zeroth-order Dead Reckoning. Holds the last defined value.
drmZero
  :: (Show v, Monad m, VectorSpace v s, HasPosition a v)
  => a -- ^ initial a to extrapolate
  -> SF m (Maybe a) a
drmZero a0 = drm (arr id) a0 (\_ -> zeroVector) getPosition (\b _ -> b)

