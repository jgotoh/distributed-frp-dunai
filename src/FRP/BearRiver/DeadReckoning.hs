{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module FRP.BearRiver.DeadReckoning
  ( drmZero
  , drmFirst
  , drmSecond
  , HasPosition(..)
  , HasVelocity(..)
  , HasAcceleration(..)
  )
where

import           Data.VectorSpace
import           Data.MonadicStreamFunction.DeadReckoning
import           FRP.BearRiver

-- Modules containing instances need FlexibleInstances and MultiParamTypeClasses language pragmas
class HasPosition a p | a -> p where
  getPosition :: a -> p

class HasVelocity a v | a -> v where
  getVelocity :: a -> v

class HasAcceleration a v | a -> v where
  getAcceleration :: a -> v

drmSecond
  :: (Show v, Monad m, VectorSpace v s, HasPosition a v, HasAcceleration a v)
  => a
  -> (a -> v -> a)
  -> SF m (Maybe a) a
drmSecond a0 new =
  drm (integral >>> integral) a0 getAcceleration getPosition new

drmFirst
  :: (Show v, Monad m, VectorSpace v s, HasPosition a v, HasVelocity a v)
  => a
  -> (a -> v -> a) -- function to construct new a from a new position
  -> SF m (Maybe a) a
drmFirst a0 new = drm integral a0 getVelocity getPosition new

drmZero :: (Show v, Monad m, VectorSpace v s, HasPosition a v) => a -> SF m (Maybe a) a
drmZero a0 = drm (arr id) a0 (\_ -> zeroVector) getPosition (\b _ -> b)

