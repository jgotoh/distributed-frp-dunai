module Game where

import           Collision
import           Display
import           GameState
import           Network.Common
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

-- executed by host that creates a session
gameSF
  :: (Monad m, MonadFix m)
  => SF (GameEnv m) (GameInput, (Maybe (StateUpdate NetState))) GameState
gameSF = feedbackM act loopingGame
 where
  act = do
    gs <- lift ask
    return (toState gs)
  toState gs = GameState (ps0 gs) (rps0 gs) (bs0 gs)
  ps0  = toPlayerState . localPlayerSettings
  bs0  = toBallState . ballSettings
  rps0 = toPlayerState . remotePlayerSettings

-- executed by host that joins a session
remoteGameSF
  :: (Monad m, MonadFix m)
  => SF (GameEnv m) (GameInput, (Maybe (StateUpdate NetState))) GameState
remoteGameSF = feedbackM act remoteLoopingGame
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
  => SF
       (GameEnv m)
       ((GameInput, (Maybe (StateUpdate NetState))), GameState)
       (GameState, GameState)
loopingGame =
  (arr gi_gs >>> morphS (selectEnv localPlayerSettings) localPlayerSF)
    &&& (arr su_gs >>> morphS (selectEnv remotePlayerSettings) remotePlayerSF)
    &&& (arr gs >>> morphS (selectEnv ballSettings) ballSF)
    >>> arr (\(ps, (ps', bs)) -> ((ps, ps'), bs))
    >>> arr ((uncurry . uncurry) GameState)
    >>> arr dup
 where
  gi_gs ((gi, _), gs') = (gi, gs')
  su_gs ((_, s), gs') = (s, gs')
  gs (_, gs') = gs'

remoteLoopingGame
  :: (Monad m)
  => SF
       (GameEnv m)
       ((GameInput, (Maybe (StateUpdate NetState))), GameState)
       (GameState, GameState)
remoteLoopingGame =
  (arr gi_gs >>> morphS (selectEnv localPlayerSettings) localPlayerSF)
    &&& (arr su_gs >>> morphS (selectEnv remotePlayerSettings) remotePlayerSF)
  -- TODO unit test when StateUpdate is Nothing -> remote ball/ player are static
    &&& (arr su_gs >>> morphS (selectEnv ballSettings) remoteBallSF)
    >>> arr (\(ps, (ps', bs)) -> ((ps, ps'), bs))
    >>> arr ((uncurry . uncurry) GameState)
    >>> arr dup
 where
  gi_gs ((gi, _), gs) = (gi, gs)
  su_gs ((_, s), gs) = (s, gs)

selectEnv
  :: (GameSettings -> a) -> ClockInfo (ReaderT a m) c -> ClockInfo (GameEnv m) c
selectEnv f = mapReaderT $ withReaderT f

localPlayerSF :: (Monad m) => SF (PlayerEnv m) (GameInput, a) PlayerState
localPlayerSF = arr fst >>> arr directionInput >>> paddleSF

-- Problem: is simulating the remotePlayer instead of just visualizing its state
remotePlayerSF
  :: (Monad m)
  => SF (PlayerEnv m) ((Maybe (StateUpdate NetState)), a) PlayerState
remotePlayerSF = arr fst >>> arr (fmap getDir) >>> paddleSF
  where getDir (StateUpdate _ s) = playerNetState s

arrTrace :: (Monad m, Show a) => MSF m a a
arrTrace = arr (\x -> trace (show x) x)

ballSF :: (Monad m) => SF (BallEnv m) GameState BallState
ballSF =
  resolveCollisions >>> feedbackM (lift $ asks ballDirection0) movingBallSF

remoteBallSF
  :: Monad m
  => SF (BallEnv m) ((Maybe (StateUpdate NetState)), GameState) BallState
remoteBallSF = arr (uncurry mergeStates) >>> arr ballState

mergeStates :: Maybe (StateUpdate NetState) -> GameState -> GameState
mergeStates mNS gs = case mNS of
  Nothing                 -> gs
  Just (StateUpdate _ ns) -> case ns of
    NetState _ bs -> case bs of
      Just bs' -> gs { ballState = bs' }
      Nothing  -> gs

resolveCollisions :: Monad m => SF (BallEnv m) GameState [Event Collision]
resolveCollisions =
  (arr ballState >>> boundsCollisionSF)
    &&& (arr localPlayerState &&& arr ballState >>> playerCollisionSF)
    &&& (arr remotePlayerState &&& arr ballState >>> playerCollisionSF)
    >>> second (arr $ uncurry (++))
    >>> (arr $ uncurry (++))

-- edge avoids multiple events for a single collision -> TODO create test
playerCollisionSF
  :: Monad m => SF (BallEnv m) (PlayerState, BallState) [Event Collision]
playerCollisionSF =
  arr (\(ps, bs) -> broadphase (toShapeBall bs) (toShapePlayer ps))
    >>> edge
    >>> arr (fmap (const PlayerCollision))
    >>> arr pure

boundsCollisionSF :: Monad m => SF (BallEnv m) BallState [Event Collision]
boundsCollisionSF =
  arr toShapeBall
    >>> arr broadphaseShape
    >>> arr (boundsColliding 0 windowWidth 0 windowHeight)
    >>> edgeJust
    >>> arr (fmap BoundsCollision)
    >>> arr pure

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

paddleSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) PlayerState
paddleSF = proc dir -> do
  c      <- colorSF -< undefined
  (p, v) <- moveSF -< dir
  b      <- constM (lift $ asks playerBounds0) -< undefined
  returnA -< PlayerState p b v c

moveSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) (Position, Velocity)
moveSF = proc dir -> do
  v    <- constM (lift $ asks playerVelocityMax) -< undefined
  p0   <- constM (lift $ asks playerPosition0) -< undefined
  dir' <- arr (fromMaybe (V2 0 0)) -< dir
  v'   <- arr (uncurry (*)) -< (v, dir')
  dp   <- integral -< v'
  p'   <- arr (uncurry (+)) -< (p0, dp)
  returnA -< (p', v')

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

colorSF :: Monad m => SF (PlayerEnv m) a Color
colorSF = constM (lift $ asks playerColor0)

