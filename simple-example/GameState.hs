
module GameState where

import           Types
import           Control.Monad.Reader
import           SDL.Vect
import           FRP.BearRiver

type GameEnv = ReaderT GameSettings

type PlayerEnv = ReaderT PlayerSettings

data GameInput = GameInput
  { jumpInput :: Bool
  } deriving (Show)

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

