module GameState where

import           Types
import           Control.Monad.Reader
import           SDL.Vect
import           FRP.BearRiver

type GameEnv = ReaderT GameSettings

type PlayerEnv = ReaderT PlayerSettings

type BallEnv = ReaderT BallSettings

data GameInput = GameInput
  { directionInput :: Maybe Direction
  } deriving (Show)

data GameSettings = GameSettings
  { localPlayerSettings :: PlayerSettings
  , ballSettings :: BallSettings
  }
  deriving (Show)

data PlayerSettings = PlayerSettings
  { playerPosition :: Position
  , playerVelocity :: Velocity
  , playerColor :: Color
  }
  deriving (Show)

data BallSettings = BallSettings
  { ballPosition :: Position
  , ballVelocity :: Velocity
  , ballColor :: Color
  }
  deriving (Show)

data GameState = GameState
  { localPlayerState :: PlayerSettings
  , ballState :: BallSettings
  }
  deriving (Show)
