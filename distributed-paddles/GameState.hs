{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GameState where

import           Collision
import           Data.Binary
import           FRP.BearRiver
import           FRP.BearRiver.DeadReckoning
import           GHC.Generics                   ( Generic )
import           Type.Reflection
import           Types
import           Control.Monad.Reader

type GameEnv = ReaderT GameSettings

type PlayerEnv = ReaderT PlayerSettings

type BallEnv = ReaderT BallSettings

data Collision = BoundsCollision Side | PlayerCollision

data GameInput = GameInput
  { directionInput :: !(Maybe Direction)
  } deriving (Show)

data GameSettings = GameSettings
  { localPlayerSettings :: PlayerSettings
  , remotePlayerSettings :: PlayerSettings
  , ballSettings :: BallSettings
  , maximumGameLength :: Time
  }
  deriving (Generic, Show, Typeable)
instance Binary GameSettings

data PlayerSettings = PlayerSettings
  { playerPosition0 :: !Position
  , playerBounds0 :: !Bounds
  , playerVelocityMax :: !Velocity
  , playerColor0 :: !Color
  }
  deriving (Generic, Show, Typeable)
instance Binary PlayerSettings

data BallSettings = BallSettings
  { ballPosition0 :: !Position
  , ballRadius0 :: !Radius
  , ballVelocityMax :: !Velocity
  , ballColor0 :: !Color
  , ballDirection0 :: !Direction
  }
  deriving (Generic, Show, Typeable)
instance Binary BallSettings

data Command = Command { dirCommand :: !Direction}
  deriving (Generic, Show, Typeable)
instance Binary Command

data NetState = NetState
  { localPlayerNetState :: !PlayerState
  , remotePlayerNetState :: !PlayerState
  , ballNetState :: !BallState
  }
  deriving (Generic, Show, Typeable)
instance Binary NetState

data GameState = GameState
  { localPlayerState :: PlayerState
  , remotePlayerState :: PlayerState
  , ballState :: BallState
  , gameOver :: Bool
  }
  deriving (Generic, Show, Typeable)
instance Binary GameState

data PlayerState = PlayerState
  { playerPositionState :: !Position
  , playerBoundsState :: !Bounds
  , playerVelocityState :: !Velocity
  , playerColorState :: !Color
  }
  deriving (Generic, Show, Typeable)
instance Binary PlayerState

instance HasPosition PlayerState Position where
  getPosition = playerPositionState

instance HasVelocity PlayerState Velocity where
  getVelocity = playerVelocityState

data BallState = BallState
  { ballPositionState :: !Position
  , ballRadiusState :: !Radius
  , ballVelocityState :: !Velocity
  , ballColorState :: !Color
  }
  deriving (Generic, Show, Typeable)
instance Binary BallState

instance HasPosition BallState Position where
  getPosition = ballPositionState

instance HasVelocity BallState Velocity where
  getVelocity = ballVelocityState

toBallState :: BallSettings -> BallState
toBallState (BallSettings p r v c _) = BallState p r v c

toPlayerState :: PlayerSettings -> PlayerState
toPlayerState (PlayerSettings p b v c) = PlayerState p b v c

toShapeBall :: BallState -> ToShape BallState
toShapeBall (BallState p r _ _) =
  ToShape { broadphaseShape = Sphere p r, narrowphaseShape = Sphere p r }

toShapePlayer :: PlayerState -> ToShape PlayerState
toShapePlayer (PlayerState p b _ _) = ToShape
  { broadphaseShape  = AABB p b
  , narrowphaseShape = AABB p b
  }


