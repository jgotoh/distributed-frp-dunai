{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GameState where

import           Control.Monad.Reader
import           SDL.Vect
import           FRP.BearRiver

type GameEnv = ReaderT GameSettings
type Position = V2 Double
type Velocity = V2 Double

type PlayerEnv = ReaderT PlayerSettings

data GameSettings = GameSettings
  { groundHeightSettings :: Double
  }

data PlayerSettings = PlayerSettings
  { playerPosition0 :: Position
  , playerVelocity0 :: Velocity
  }

data GameState = GameState
  { leftBallPosState :: Position
  } deriving (Show)

-- TODO orphaninstance
instance RealFloat a => VectorSpace (V2 a) a where
  zeroVector = zero

  a *^ v = a SDL.Vect.*^ v

  v ^/ a = v SDL.Vect.^/ a

  negateVector = SDL.Vect.negated

  v1 ^+^ v2 = v1 SDL.Vect.^+^ v2

  v1 ^-^ v2 = v1 SDL.Vect.^-^ v2

  v1 `dot` v2 = SDL.Vect.dot v1 v2


