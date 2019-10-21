{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import           Data.Word
import           FRP.BearRiver
import qualified SDL
import qualified SDL.Primitive
import           SDL.Vect

type Position = V2 Double
type Velocity = V2 Double
type Color = SDL.Primitive.Color

-- TODO orphaninstance
instance RealFloat a => VectorSpace (V2 a) a where
  zeroVector = zero

  a *^ v = a SDL.Vect.*^ v

  v ^/ a = v SDL.Vect.^/ a

  negateVector = SDL.Vect.negated

  v1 ^+^ v2 = v1 SDL.Vect.^+^ v2

  v1 ^-^ v2 = v1 SDL.Vect.^-^ v2

  v1 `dot` v2 = SDL.Vect.dot v1 v2

