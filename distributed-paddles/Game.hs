module Game where

import GameState
import Input
import Types

import Control.Monad.Reader (lift)
import Control.Monad.Trans.MSF.Reader
import Debug.Trace
import FRP.BearRiver hiding (dot, (^+^))
import SDL.Vect hiding (identity, trace)


gameSF :: Monad m => SF (GameEnv m) GameInput GameState
gameSF = (arr directionInput >>> (morphS (selectEnv localPlayerSettings) paddleSF)) &&& (morphS (selectEnv ballSettings) $ feedback ballDir0 ballSF) >>> arr (uncurry GameState)
  where
    ballDir0 = V2 0.75 $ -0.12

selectEnv :: (GameSettings -> a) -> ClockInfo (ReaderT a m) c -> ClockInfo (GameEnv m) c
selectEnv f = mapReaderT $ withReaderT f

paddleSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) PlayerSettings
paddleSF = proc dir -> do
  c <- colorSF -< undefined
  (p, v) <- moveSF -< dir
  returnA -< PlayerSettings p v c

moveSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) (Position, Velocity)
moveSF = proc (dir) -> do
  v <- constM (lift $ asks playerVelocity) -< undefined
  p0 <- constM (lift $ asks playerPosition) -< undefined
  dir' <- arr (maybe (V2 0 0) id ) -< dir
  v' <- arr (\(v, dir') -> v * dir') -< (v, dir')
  dp <- integral -< v'
  p' <- arr (\(p0, dp) -> p0 + dp) -< (p0, dp)
  returnA -< (p', v)
  where
    dirToInt d = case d of
      Nothing -> V2 0 0
      Just a -> a

ballSF :: Monad m => SF (BallEnv m) (GameInput, Direction) (BallSettings, Direction)
ballSF = proc (_, dir) -> do
  c <- morphS bsToPs colorSF -< undefined
  (p, v) <- morphS bsToPs moveSF -< Just dir
  returnA -< (BallSettings p v c, dir)
  where
    bsToPs = mapReaderT $ withReaderT ps
    ps (BallSettings p v c) = PlayerSettings p v c


staticToMove :: Monad m => Position -> MSF (ClockInfo (PlayerEnv m)) (Maybe Direction) (Position, Velocity)
staticToMove p0 = switch (arr (\_ -> (p0, V2 0.0 0.0)) &&& isMoving) (moveToStatic p0)

-- TODO move in direction dir with velocity asks playerVelocity
moveToStatic :: Monad m => Position -> Direction -> SF (PlayerEnv m) (Maybe Direction) (Position, V2 Double)
moveToStatic p0 dir = switch ((move p0 dir) &&& stopsMoving) (\_ -> staticToMove p0)

move :: Position -> Direction -> MSF (ClockInfo (PlayerEnv m)) (Maybe Direction) (Position, V2 Double)
move p0 dir = undefined

stopsMoving :: Monad m => MSF (ClockInfo m) (Maybe Direction) (Event a)
stopsMoving = arr (\dir -> case dir of
                      Just dir -> NoEvent
                      Nothing -> Event undefined)


colorSF :: Monad m => SF (PlayerEnv m) a Color
colorSF = constM (lift $ asks playerColor)

constPosSF :: Monad m => SF (PlayerEnv m) a Position
constPosSF = constM (lift $ asks playerPosition)

isMoving :: Monad m => SF m (Maybe Direction) (Event Direction)
isMoving = arr maybeToEvent

constPosition :: Monad m => Position -> SF m a Position
constPosition p = constM $ return p

