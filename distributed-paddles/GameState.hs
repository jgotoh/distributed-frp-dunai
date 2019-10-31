{-# LANGUAGE DeriveGeneric #-}

module GameState where

import           Collision
import           Data.Binary
import           GHC.Generics                   ( Generic )
import           Type.Reflection
import           Types
import           Control.Monad.Reader
import           SDL.Vect

type GameEnv = ReaderT GameSettings

type PlayerEnv = ReaderT PlayerSettings

type BallEnv = ReaderT BallSettings

data Collision = BoundsCollision Side | PlayerCollision

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
  , playerBounds :: Bounds
  , playerVelocity :: Velocity
  , playerColor :: Color
  }
  deriving (Generic, Show, Typeable)
instance Binary PlayerSettings

data BallSettings = BallSettings
  { ballPosition :: Position
  , ballRadius :: Radius
  , ballVelocity :: Velocity
  , ballColor :: Color
  }
  deriving (Generic, Show, Typeable)
instance Binary BallSettings

data GameState = GameState
  { localPlayerState :: PlayerSettings
  , ballState :: BallSettings
  }
  deriving (Generic, Show, Typeable)
instance Binary GameState

toShapeBall :: BallSettings -> ToShape BallSettings
toShapeBall (BallSettings p r _ _) =
  ToShape { broadphaseShape = Sphere p r, narrowphaseShape = Sphere p r }

toShapePlayer :: PlayerSettings -> ToShape PlayerSettings
toShapePlayer (PlayerSettings p b _ _) = ToShape
  { broadphaseShape  = Sphere center radius
  , narrowphaseShape = AABB p b
  }
 where
  radius = case b of
    V2 x y -> max x y
  center = p + 0.5 * b


