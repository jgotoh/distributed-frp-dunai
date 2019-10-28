module Game where

import Collision
import Display
import GameState
import Types

import Debug.Trace
import Control.Monad.Fix
import Control.Monad.Reader (lift)
import Control.Monad.Trans.MSF.Reader
import FRP.BearRiver hiding (dot, (^+^))
import SDL.Vect hiding (identity, trace)
import Data.MonadicStreamFunction.InternalCore

-- TODO move to dunai/Extra.hs
-- | Well-formed looped connection of an output component as a future input.
-- Receives initial input via monadic action
feedbackM :: Monad m => m c -> MSF m (a, c) (b, c) -> MSF m a b
feedbackM act sf = MSF $ \a -> do
  c <- act
  ((b', c'), sf') <- unMSF sf (a, c)
  return (b', feedback c' sf')

-- TODO move to bearriver/Extra.hs
edgeJust :: Monad m => SF m (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
    where
        isJustEdge Nothing  Nothing     = Nothing
        isJustEdge Nothing  ma@(Just _) = ma
        isJustEdge (Just _) (Just _)    = Nothing
        isJustEdge (Just _) Nothing     = Nothing

gameSF :: (Monad m, MonadFix m) => SF (GameEnv m) GameInput GameState
gameSF = feedbackM (act) loopingGame
  where
    act =  do
      gs <- lift $ ask
      return (toState gs)
    toState gs = GameState (ps0 gs) (bs0 gs)
    ps0 = localPlayerSettings
    bs0 = ballSettings

loopingGame :: (Monad m) => SF (GameEnv m) (GameInput, GameState) (GameState, GameState)
loopingGame = (morphS (selectEnv localPlayerSettings) localPlayerSF) &&& (morphS (selectEnv ballSettings) ballSF) >>> arr (uncurry GameState) >>> arr dup

selectEnv :: (GameSettings -> a) -> ClockInfo (ReaderT a m) c -> ClockInfo (GameEnv m) c
selectEnv f = mapReaderT $ withReaderT f

localPlayerSF :: (Monad m) => SF (PlayerEnv m) (GameInput, GameState) PlayerSettings
localPlayerSF = arr fst >>> arr directionInput >>> (paddleSF)

ballSF :: (Monad m) => SF (BallEnv m) (GameInput, GameState) BallSettings
ballSF = second resolveCollisions >>> feedback ballDir0 movingBallSF
  where
    -- TODO replace ballDir0 with feedbackM usage!
    ballDir0 = V2 (- 0.75) $ -0.12

resolveCollisions :: Monad m => SF (BallEnv m) GameState (Collisions Collision)
resolveCollisions = (arr localPlayerState &&& arr ballState >>> playerCollisionSF) &&& (arr ballState >>> boundsCollisionSF) >>> arr concat

-- edge avoids multiple events for a single collision -> TODO create test
playerCollisionSF :: Monad m => SF (BallEnv m) (PlayerSettings, BallSettings) [Event Collision]
playerCollisionSF = arr (\(ps, bs) -> broadphase (toShapeBall bs) (toShapePlayer ps)) >>> edge >>> arr (fmap (\_ -> PlayerCollision)) >>> arr pure

boundsCollisionSF :: Monad m => SF (BallEnv m) BallSettings [Event Collision]
boundsCollisionSF = arr (\bs -> (toShapeBall bs)) >>> arr broadphaseShape >>> arr (boundsColliding 0 windowWidth 0 windowHeight) >>> edgeJust >>> arr (fmap $ BoundsCollision) >>> arr pure

collisionSF :: Monad m => SF (BallEnv m) (Direction, Collisions Collision) Direction
collisionSF = arr (\(dir, cs) -> foldl applyCollision dir (cs' cs) )
  where
    applyCollision dir c = case c of
      PlayerCollision -> dir * (V2 (-1) 1)
      BoundsCollision side -> dir * dirChange side
    cs' cs = fromEvent <$> (filter isEvent cs)
    dirChange side = case side of
      TopSide -> V2 1 (-1)
      BottomSide -> V2 1 (-1)
      LeftSide -> V2 (-1) 1
      RightSide -> V2 (-1) 1

movingBallSF :: Monad m => SF (BallEnv m) ((GameInput, Collisions Collision), Direction) (BallSettings, Direction)
movingBallSF = proc ((_, cs), dir) -> do
  c <- morphS bsToPs colorSF -< undefined
  dir' <- collisionSF -< (dir, cs)
  (p, v) <- morphS bsToPs moveSF -< Just dir'
  b <- constM (lift $ asks ballRadius) -< undefined
  returnA -< (BallSettings p b v c, trace (show dir') (dir'))
  where
    bsToPs = mapReaderT $ withReaderT ps
    ps (BallSettings p b v c) = PlayerSettings p (V2 b b) v c

paddleSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) PlayerSettings
paddleSF = proc dir -> do
  c <- colorSF -< undefined
  (p, v) <- moveSF -< dir
  b <- constM (lift $ asks playerBounds) -< undefined
  returnA -< PlayerSettings p b v c

moveSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) (Position, Velocity)
moveSF = proc (dir) -> do
  v <- constM (lift $ asks playerVelocity) -< undefined
  p0 <- constM (lift $ asks playerPosition) -< undefined
  dir' <- arr (maybe (V2 0 0) id ) -< dir
  v' <- arr (\(v, dir') -> v * dir') -< (v, dir')
  dp <- integral -< v'
  p' <- arr (\(p0, dp) -> p0 + dp) -< (p0, dp)
  returnA -< (p', v)

colorSF :: Monad m => SF (PlayerEnv m) a Color
colorSF = constM (lift $ asks playerColor)

