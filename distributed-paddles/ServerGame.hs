{-# LANGUAGE LambdaCase #-}

module ServerGame where

import Collision
import           Display
import Types
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import           FRP.BearRiver
import           FRP.BearRiver.Extra
import           Data.MonadicStreamFunction.Extra
import           Control.Monad.Trans.MSF.Reader
import           Network.Common
import qualified Control.Distributed.Process   as P
import           GameState
import           FRP.BearRiver           hiding ( dot
                                                , (^+^)
                                                )
import           SDL.Vect                hiding ( identity
                                                , trace
                                                )
import           Control.Monad.Reader           ( lift )

data ObjectType = LocalPlayer | RemotePlayer
  deriving (Show, Eq, Ord)

serverSF
  :: Monad m
  => Map.Map ObjectType P.ProcessId
  -> SF (GameEnv m) (GameInput, (Maybe [CommandPacket Command])) GameState
serverSF pids = feedbackM act (loopingGame pids)
 where
  act = do
    gs <- lift ask
    return (toState gs)
  toState gs = GameState (ps0 gs) (rps0 gs) (bs0 gs)
  ps0  = toPlayerState . localPlayerSettings
  bs0  = toBallState . ballSettings
  rps0 = toPlayerState . remotePlayerSettings

loopingGame
  :: (Monad m)
  => Map.Map ObjectType P.ProcessId
  -> SF
       (GameEnv m)
       ((GameInput, (Maybe [CommandPacket Command])), GameState)
       (GameState, GameState)
loopingGame pids =
  (arr (snd . fst) >>> arr (selectCmd (pids!LocalPlayer)) >>> morphS (selectEnv localPlayerSettings) playerSF)
  &&& (arr (snd . fst) >>> arr (selectCmd (pids!RemotePlayer)) >>> morphS (selectEnv remotePlayerSettings) playerSF)
  &&& (arr snd >>> morphS (selectEnv ballSettings) ballSF)
  >>> arr (\(ps, (rps, bs)) -> ((ps, rps), bs))
  >>> arr ((uncurry . uncurry) GameState)
  >>> arr dup

selectCmd :: P.ProcessId -> Maybe [CommandPacket Command] -> Maybe (CommandPacket Command)
selectCmd pid xs = (filter has) <$> xs >>=
  (\xs' -> case xs' of
      [] -> Nothing
      (x:_) -> Just x)
  where
    has cmd = case cmd of
      CommandPacket pid' _ -> pid' == pid

selectEnv
  :: (GameSettings -> a) -> ClockInfo (ReaderT a m) c -> ClockInfo (GameEnv m) c
selectEnv f = mapReaderT $ withReaderT f

playerSF
        :: Monad m => MSF (ReaderT DTime (ReaderT PlayerSettings m)) (Maybe (CommandPacket Command)) PlayerState
playerSF = arr mapCommand >>> paddleSF
  where
    mapCommand = arr (fmap dirCommand . command)
    command = fmap (\case CommandPacket _ c -> c)

paddleSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) PlayerState
paddleSF = proc dir -> do
  c      <- colorSF -< undefined
  (p, v) <- moveSF -< dir
  b      <- constM (lift $ asks playerBounds0) -< undefined
  returnA -< PlayerState p b v c

ballSF :: (Monad m) => SF (BallEnv m) GameState BallState
ballSF =
  resolveCollisions >>> feedbackM (lift $ asks ballDirection0) movingBallSF

movingBallSF
  :: Monad m
  => SF (BallEnv m) (Collisions Collision, Direction) (BallState, Direction)
movingBallSF = proc (cs, dir) -> do
  c      <- morphS bsToPs colorSF -< undefined
  dir'   <- applyCollisionSF -< (dir, cs)
  (p, v) <- morphS bsToPs moveSF -< Just dir'
  b      <- constM (lift $ asks ballRadius0) -< undefined
  returnA -< (BallState p b v c, dir')
 where
  bsToPs = mapReaderT $ withReaderT ps
  ps (BallSettings p b v c _) = PlayerSettings p (V2 b b) v c

moveSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) (Position, Velocity)
moveSF = proc dir -> do
  v    <- constM (lift $ asks playerVelocityMax) -< undefined
  p0   <- constM (lift $ asks playerPosition0) -< undefined
  dir' <- arr (fromMaybe (V2 0 0)) -< dir
  v'   <- arr (uncurry (*)) -< (v, dir')
  dp   <- integral -< v'
  p'   <- arr (uncurry (+)) -< (p0, dp)
  returnA -< (p', v')

-- CollisionSFs
resolveCollisions :: Monad m => SF (BallEnv m) GameState [Event Collision]
resolveCollisions =
  (arr ballState >>> boundsCollisionSF)
    &&& (arr localPlayerState &&& arr ballState >>> playerCollisionSF)
    &&& (arr remotePlayerState &&& arr ballState >>> playerCollisionSF)
    >>> second (arr $ uncurry (++))
    >>> (arr $ uncurry (++))

boundsCollisionSF :: Monad m => SF (BallEnv m) BallState [Event Collision]
boundsCollisionSF =
  arr toShapeBall
    >>> arr broadphaseShape
    >>> arr (boundsColliding 0 windowWidth 0 windowHeight)
    >>> edgeJust
    >>> arr (fmap BoundsCollision)
    >>> arr pure

applyCollisionSF
  :: Monad m => SF (BallEnv m) (Direction, Collisions Collision) Direction
applyCollisionSF = arr (\(dir, cs) -> foldl applyCollision dir (cs' cs))
 where
  applyCollision dir c = case c of
    PlayerCollision      -> dir * V2 (-1) 1
    BoundsCollision side -> dir * dirChange side
  cs' cs = fromEvent <$> filter isEvent cs
  dirChange side = case side of
    TopSide    -> V2 1 (-1)
    BottomSide -> V2 1 (-1)
    LeftSide   -> V2 (-1) 1
    RightSide  -> V2 (-1) 1

playerCollisionSF
  :: Monad m => SF (BallEnv m) (PlayerState, BallState) [Event Collision]
playerCollisionSF =
  arr (\(ps, bs) -> broadphase (toShapeBall bs) (toShapePlayer ps))
    >>> edge
    >>> arr (fmap (const PlayerCollision))
    >>> arr pure

-- Various SFs
colorSF :: Monad m => SF (PlayerEnv m) a Color
colorSF = constM (lift $ asks playerColor0)