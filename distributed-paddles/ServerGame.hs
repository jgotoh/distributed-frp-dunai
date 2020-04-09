{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module ServerGame
  ( ObjectType(..)
  , serverSF
  , serverSFWarp
  , paddleSF
  )
where

import           Collision
import           Display
import           Types
import           Data.Maybe
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( (!) )
import           Data.MonadicStreamFunction.Network.TimeWarp
import           Data.MonadicStreamFunction.Extra
import           Control.Monad.Trans.MSF.Reader
import           Network.Common
import qualified Control.Distributed.Process   as P
import           GameState
import           FRP.BearRiver           hiding ( dot
                                                , (^+^)
                                                , normalize
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
  -> SF (GameEnv m) (GameInput, [CommandPacket Command]) GameState
serverSF pids = feedbackM act (loopingGame pids) >>> checkTime
 where
  act = do
    gs <- lift ask
    return (toState gs)
  toState gs = GameState (ps0 gs) (rps0 gs) (bs0 gs) False
  ps0  = toPlayerState . localPlayerSettings
  bs0  = toBallState . ballSettings
  rps0 = toPlayerState . remotePlayerSettings

checkTime :: Monad m => SF (GameEnv m) GameState GameState
checkTime = arr id &&& check >>> arr (\(gs, q) -> gs { gameOver = q })
 where
  check =
    constM (lift $ asks maximumGameLength) &&& time >>> arr (uncurry (<=))

serverSFWarp
  :: Monad m
  => Map.Map ObjectType P.ProcessId
  -> Natural
  -> SF
       (GameEnv m)
       (Natural, (GameInput, [CommandPacket Command]))
       GameState
serverSFWarp pids frames =
  rollbackMSF frames $ feedbackM act (loopingGame pids) >>> checkTime
 where
  act = do
    gs <- lift ask
    return (toState gs)
  toState gs = GameState (ps0 gs) (rps0 gs) (bs0 gs) False
  ps0  = toPlayerState . localPlayerSettings
  bs0  = toBallState . ballSettings
  rps0 = toPlayerState . remotePlayerSettings

loopingGame
  :: (Monad m)
  => Map.Map ObjectType P.ProcessId
  -> SF
       (GameEnv m)
       ((GameInput, ([CommandPacket Command])), GameState)
       (GameState, GameState)
loopingGame pids =
  (   arr (snd . fst)
    >>> arr (selectCmd (pids ! LocalPlayer))
    >>> morphS (selectEnv localPlayerSettings) playerSF
    )
    &&& (   arr (snd . fst)
        >>> arr (selectCmd (pids ! RemotePlayer))
        >>> morphS (selectEnv remotePlayerSettings) playerSF
        )
    &&& (arr snd >>> ballSF)
    >>> arr (\(ps, (rps, bs)) -> ((ps, rps), bs))
    >>> arr ((uncurry . uncurry) GameState)
    >>> arr (\gs -> gs False)
    >>> arr dup

selectCmd
  :: P.ProcessId -> [CommandPacket Command] -> Maybe (CommandPacket Command)
selectCmd pid xs = case filter has xs of
  []      -> Nothing
  (x : _) -> Just x
 where
  has cmd = case cmd of
    CommandPacket pid' _ _ -> pid' == pid

selectEnv
  :: (GameSettings -> a) -> ClockInfo (ReaderT a m) c -> ClockInfo (GameEnv m) c
selectEnv f = mapReaderT $ withReaderT f

playerSF
  :: Monad m
  => MSF
       (ReaderT DTime (ReaderT PlayerSettings m))
       (Maybe (CommandPacket Command))
       PlayerState
playerSF = arr mapCommand >>> paddleSF
 where
  mapCommand = arr (fmap dirCommand . command)
  command    = fmap
    (\case
      CommandPacket _ _ c -> c
    )

paddleSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) PlayerState
paddleSF = proc dir -> do
  c      <- colorSF -< ()
  (p, v) <- moveSF -< dir
  b      <- constM (lift $ asks playerBounds0) -< ()
  returnA -< PlayerState p b v c

ballSF :: (Monad m) => SF (GameEnv m) GameState BallState
ballSF = switch
  (   arr id
  &&& (morphS
        (selectEnv ballSettings)
        (   resolveCollisions
        >>> feedbackM (lift $ asks ballDirection0) movingBallSF
        )
      )
  >>> arr snd
  &&& checkScore
  )
  (\x -> morphS (mapReaderT (local $ newVelocity x)) ballSF)

newVelocity :: (ObjectType, Direction) -> GameSettings -> GameSettings
newVelocity x gs = gs
  { ballSettings = bs { ballDirection0 = dir', ballVelocityMax = vel' }
  }
 where
  bs   = ballSettings gs
  dir' = snd x
  vel' = (ballVelocityMax bs) * 1.01

movingBallSF
  :: Monad m
  => SF (BallEnv m) (Collisions Collision, Direction) (BallState, Direction)
movingBallSF = proc (cs, dir) -> do
  c      <- morphS bsToPs colorSF -< ()
  dir'   <- applyCollisionSF -< (dir, cs)
  (p, v) <- morphS bsToPs moveSF -< Just dir'
  r      <- constM (lift $ asks ballRadius0) -< ()
  returnA -< (BallState p r v c, dir')
 where
  bsToPs = mapReaderT $ withReaderT ps
  ps (BallSettings p b v c _) = PlayerSettings p (V2 b b) v c

-- checks if a player scored. Event contains the scored player and an updated direction for the ball after reset.
checkScore
  :: Monad m
  => SF (GameEnv m) (GameState, BallState) (Event (ObjectType, Direction))
checkScore = proc (gs,bs) -> do
  localX  <- constM (lift $ asks $ playerX localPlayerSettings) -< ()
  remoteX <- constM (lift $ asks $ playerX remotePlayerSettings) -< ()
  ballX   <-
    arr
        ( (\p -> case p of
            V2 x _ -> x
          )
        . ballPositionState
        )
      -< bs
  ev <- arr check -< (ballX, localX, remoteX)
  returnA -< newDir gs <$> ev
 where
  playerX player =
    (\p -> case p of
        V2 x _ -> x
      )
      . playerPosition0
      . player
  check (ballX, localX, remoteX) = if
    | ballX < localX  -> Event RemotePlayer
    | ballX > remoteX -> Event LocalPlayer
    | otherwise       -> NoEvent

newDir :: GameState -> ObjectType -> (ObjectType, Direction)
newDir gs o = (o, dir')
 where
  dir'   = normalize $ a - b
  (a, b) = case o of
    RemotePlayer -> (remotePos, localPos)
    LocalPlayer  -> (localPos, remotePos)
  localPos  = playerPositionState $ localPlayerState gs
  remotePos = playerPositionState $ remotePlayerState gs

moveSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) (Position, Velocity)
moveSF = proc dir -> do
  v    <- constM (lift $ asks playerVelocityMax) -< ()
  p0   <- constM (lift $ asks playerPosition0) -< ()
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
