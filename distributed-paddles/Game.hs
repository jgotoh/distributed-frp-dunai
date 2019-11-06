module Game where

import           Collision
import           Display
import           GameState
import Network.Common
import           Types

import           Data.Maybe
import           Debug.Trace
import           Control.Monad.Fix
import           Control.Monad.Reader           ( lift )
import           Control.Monad.Trans.MSF.Reader
import           FRP.BearRiver           hiding ( dot
                                                , (^+^)
                                                )
import           SDL.Vect                hiding ( identity
                                                , trace
                                                )
import           Data.MonadicStreamFunction.InternalCore

-- TODO move to dunai/Extra.hs
-- | Well-formed looped connection of an output component as a future input.
-- Receives initial input via monadic action
feedbackM :: Monad m => m c -> MSF m (a, c) (b, c) -> MSF m a b
feedbackM act sf = MSF $ \a -> do
  c               <- act
  ((b', c'), sf') <- unMSF sf (a, c)
  return (b', feedback c' sf')

-- TODO move to bearriver/Extra.hs
edgeJust :: Monad m => SF m (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
 where
  isJustEdge Nothing  Nothing     = Nothing
  isJustEdge Nothing  ma@(Just _) = ma
  isJustEdge (Just _) (   Just _) = Nothing
  isJustEdge (Just _) Nothing     = Nothing

gameSF :: (Monad m, MonadFix m) => SF (GameEnv m) (GameInput, (Maybe (StateUpdate GameState))) GameState
gameSF = feedbackM act loopingGame
 where
  act = do
    gs <- lift ask
    return (toState gs)
  toState gs = GameState (ps0 gs) (rps0 gs) (bs0 gs)
  ps0 = localPlayerSettings
  bs0 = ballSettings
  rps0 = remotePlayerSettings

remoteGameSF :: (Monad m, MonadFix m) => SF (GameEnv m) (GameInput, (Maybe (StateUpdate GameState))) GameState
remoteGameSF = feedbackM act remoteLoopingGame
 where
  act = do
    gs <- lift ask
    return (toState gs)
  toState gs = GameState (ps0 gs) (rps0 gs) (bs0 gs)
  ps0 = localPlayerSettings
  bs0 = ballSettings
  rps0 = remotePlayerSettings

loopingGame
  :: (Monad m) => SF (GameEnv m) ((GameInput, (Maybe (StateUpdate GameState))), GameState) (GameState, GameState)
loopingGame =
  (arr gi_gs >>> morphS (selectEnv localPlayerSettings) localPlayerSF)
    &&& (arr su_gs >>> morphS (selectEnv remotePlayerSettings) remotePlayerSF)
    &&& (arr gi_gs >>> morphS (selectEnv ballSettings) ballSF)
    >>> arr (\(ps, (ps', bs)) -> ((ps, ps'), bs))
    >>> arr ((uncurry . uncurry) GameState)
    >>> arr dup
  where
    gi_gs ((gi, _), gs) = (gi, gs)
    su_gs ((_, s), gs)= (s, gs)

remoteLoopingGame
  :: (Monad m) => SF (GameEnv m) ((GameInput, (Maybe (StateUpdate GameState))), GameState) (GameState, GameState)
remoteLoopingGame =
  (arr gi_gs >>> morphS (selectEnv localPlayerSettings) localPlayerSF)
    &&& (arr su_gs >>> morphS (selectEnv remotePlayerSettings) remotePlayerSF)
  -- TODO unit test su is Nothing -> static remote ball/ player
    &&& (arr su_gs >>> morphS (selectEnv ballSettings) remoteBallSF)
    >>> arr (\(ps, (ps', bs)) -> ((ps, ps'), bs))
    >>> arr ((uncurry . uncurry) GameState)
    >>> arr dup
  where
    gi_gs ((gi, _), gs) = (gi, gs)
    su_gs ((_, s), gs)= (s, gs)

selectEnv
  :: (GameSettings -> a) -> ClockInfo (ReaderT a m) c -> ClockInfo (GameEnv m) c
selectEnv f = mapReaderT $ withReaderT f

localPlayerSF
  :: (Monad m) => SF (PlayerEnv m) (GameInput, a) PlayerSettings
localPlayerSF = arr fst >>> arr directionInput >>> paddleSF

remotePlayerSF :: (Monad m) => SF (PlayerEnv m) ((Maybe (StateUpdate GameState)), a) PlayerSettings
remotePlayerSF = arr fst >>> arr (fmap getDir) >>> arrLog >>> paddleSF
  where getDir (StateUpdate _ gs) = SDL.Vect.normalize (playerVelocity . localPlayerState $ gs)

arrLog :: (Monad m, Show a) => MSF m a a
arrLog = arr (\x -> trace (show x) x)

ballSF :: (Monad m) => SF (BallEnv m) (a, GameState) BallSettings
ballSF = second resolveCollisions >>> feedback ballDir0 movingBallSF
  where
    -- TODO replace ballDir0 with feedbackM usage! create BallState adt
        ballDir0 = V2 (-0.75) $ -0.12

remoteBallSF :: (Monad m) => SF (BallEnv m) ((Maybe (StateUpdate GameState)), GameState) BallSettings
remoteBallSF = second resolveCollisions >>> arr id >>> feedback ballDir0 movingBallSF
  -- TODO combine su with gs
  where
    ballDir0 = V2 0 0

resolveCollisions :: Monad m => SF (BallEnv m) GameState [Event Collision]
resolveCollisions =
  (arr ballState >>> boundsCollisionSF)
    &&& (arr localPlayerState &&& arr ballState >>> playerCollisionSF)
    -- &&& (arr remotePlayerState &&& arr ballState >>> playerCollisionSF)
    >>> arr (uncurry (++)) --foldColl --

-- edge avoids multiple events for a single collision -> TODO create test
playerCollisionSF
  :: Monad m => SF (BallEnv m) (PlayerSettings, BallSettings) [Event Collision]
playerCollisionSF =
  arr (\(ps, bs) -> broadphase (toShapeBall bs) (toShapePlayer ps))
    >>> edge
    >>> arr (fmap (const PlayerCollision))
    >>> arr pure

boundsCollisionSF :: Monad m => SF (BallEnv m) BallSettings [Event Collision]
boundsCollisionSF =
  arr toShapeBall
    >>> arr broadphaseShape
    >>> arr (boundsColliding 0 windowWidth 0 windowHeight)
    >>> edgeJust
    >>> arr (fmap BoundsCollision)
    >>> arr pure

collisionSF
  :: Monad m => SF (BallEnv m) (Direction, Collisions Collision) Direction
collisionSF = arr (\(dir, cs) -> foldl applyCollision dir (cs' cs))
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

movingBallSF
  :: Monad m
  => SF
       (BallEnv m)
       ((a, Collisions Collision), Direction)
       (BallSettings, Direction)
movingBallSF = proc ((_, cs), dir) -> do
  c      <- morphS bsToPs colorSF -< undefined
  dir'   <- collisionSF -< (dir, cs)
  (p, v) <- morphS bsToPs moveSF -< Just dir'
  b      <- constM (lift $ asks ballRadius) -< undefined
  returnA -< (BallSettings p b v c, dir')
 where
  bsToPs = mapReaderT $ withReaderT ps
  ps (BallSettings p b v c) = PlayerSettings p (V2 b b) v c

paddleSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) PlayerSettings
paddleSF = proc dir -> do
  c      <- colorSF -< undefined
  (p, v) <- moveSF -< dir
  b      <- constM (lift $ asks playerBounds) -< undefined
  returnA -< PlayerSettings p b v c

moveSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) (Position, Velocity)
moveSF = proc dir -> do
  v    <- constM (lift $ asks playerVelocity) -< undefined
  p0   <- constM (lift $ asks playerPosition) -< undefined
  dir' <- arr (fromMaybe (V2 0 0)) -< dir
  v'   <- arr (uncurry (*)) -< (v, dir')
  dp   <- integral -< v'
  p'   <- arr (uncurry (+)) -< (p0, dp)
  returnA -< (p', v)

colorSF :: Monad m => SF (PlayerEnv m) a Color
colorSF = constM (lift $ asks playerColor)

