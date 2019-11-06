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
  , remotePlayerSettings :: PlayerSettings
  , ballSettings :: BallSettings
  }
  deriving (Show)

data PlayerSettings = PlayerSettings
  { playerPosition0 :: Position
  , playerBounds0 :: Bounds
  , playerVelocityMax :: Velocity
  , playerColor0 :: Color
  }
  deriving (Generic, Show, Typeable)
instance Binary PlayerSettings

data BallSettings = BallSettings
  { ballPosition0 :: Position
  , ballRadius0 :: Radius
  , ballVelocityMax :: Velocity
  , ballColor0 :: Color
  }
  deriving (Generic, Show, Typeable)
instance Binary BallSettings


-- TODO aggregate state records in ADT
data GameState = GameState
  { localPlayerState :: PlayerState
  , remotePlayerState :: PlayerState
  , ballState :: BallState
  }
  deriving (Generic, Show, Typeable)
instance Binary GameState

data PlayerState = PlayerState
  { playerPositionState :: Position
  , playerBoundsState :: Bounds
  , playerVelocityState :: Velocity
  , playerColorState :: Color
  }
  deriving (Generic, Show, Typeable)
instance Binary PlayerState

data BallState = BallState
  { ballPositionState :: Position
  , ballRadiusState :: Radius
  , ballVelocityState :: Velocity
  , ballColorState :: Color
  }
  deriving (Generic, Show, Typeable)
instance Binary BallState

toBallState :: BallSettings -> BallState
toBallState (BallSettings p r v c) = BallState p r v c

toPlayerState :: PlayerSettings -> PlayerState
toPlayerState (PlayerSettings p b v c) = PlayerState p b v c

toShapeBall :: BallState -> ToShape BallState
toShapeBall (BallState p r _ _) =
  ToShape { broadphaseShape = Sphere p r, narrowphaseShape = Sphere p r }

toShapePlayer :: PlayerState -> ToShape PlayerState
toShapePlayer (PlayerState p b _ _) = ToShape
  { broadphaseShape  = Sphere center radius
  , narrowphaseShape = AABB p b
  }
 where
  radius = case b of
    V2 x y -> max x y
  center = p + 0.5 * b


