{-# LANGUAGE TupleSections #-}

module Game where

import GameState

import Control.Monad.Reader (lift)
import Control.Monad.Trans.MSF.Reader
import FRP.BearRiver hiding ((^+^))
import SDL.Vect

gameSF :: Monad m => SF m a GameState
gameSF = constM ask >>> arr (, ())
  >>> runReaderS_ (runReaderS_ (runReaderS bouncingBall) gs) ps >>> arr GameState
    where
      gs = GameSettings 50
      ps = PlayerSettings (V2 100 75) (V2 0 100)

gravity :: Velocity
gravity = V2 0 (-150)

movingBall :: Monad m => SF (PlayerEnv m) () Position
movingBall = proc () -> do
  dv <- integral -< gravity
  v1 <- arrM(\dv -> (dv +) <$> lift (asks playerVelocity0)) -< dv
  dp <- integral -< v1
  pos1 <- arrM(\dp -> (dp +) <$> lift (asks playerPosition0)) -< dp
  returnA -< pos1
    where
      add :: Double -> Velocity -> Velocity
      add z v = case v of V2 x y -> V2 x $ y+z

hitGround :: Monad m => SF (GameEnv m) Position Bool
hitGround = arrM $ \b -> (yOfV2 b <=) <$> lift (asks groundHeightSettings)
  where
    yOfV2 v = case v of V2 _ y -> y

-- Converts sf to stream of events. Events hold values when sf returned true.
edgeSF :: Monad m => SF m a Bool -> SF m a (Event a)
edgeSF sf = (arr id &&& sf) >>> arr (\(x, bool) -> if bool then Event x else NoEvent)

-- A ball that bounces back up when it hit ground
bouncingBall :: Monad m => SF (GameEnv (PlayerEnv m)) () Position
bouncingBall = switch (liftTransSF movingBall >>> (arr id &&& edgeSF hitGround)) liftBouncingBallPS

-- Flattens a ReaderT r1 to arrow input and gives rise to an outer Reader r2
liftInner :: Monad m => MSF (ReaderT r1 m) a b -> MSF (ReaderT r2 m) (r1, a) b
liftInner sf = readerS $ addInput $ runReaderS sf

-- Creates a ReaderT in an inner layer, lifts sf up in the Monad stack
liftTransSF :: Monad m => MSF (ReaderT t1 m) a b -> MSF (ReaderT t1 (ReaderT t2 m)) a b
liftTransSF sf = readerS $ liftInner sf

-- Send an additional argument into an arrow
addInput :: Monad m => MSF m a b -> MSF m (c,a) b
addInput sf = second sf >>> arr snd

-- Const position p0
constBall :: Monad m => Position -> SF m () Position
constBall p0 = constM $ return p0

-- Lifts bouncingBallPS back to SF
liftBouncingBallPS :: Monad m => Position -> SF (GameEnv (PlayerEnv m)) () Position
liftBouncingBallPS p = readerS $ readerS $ bouncingBallPS p

-- Runs bouncingBall at a different initial position p
bouncingBallPS :: Monad m => Position -> MSF (PlayerEnv m) (GameSettings, (DTime, ())) Position
bouncingBallPS p = runReaderS_ (runReaderS $ runReaderS bouncingBall) ps
  where
    ps = PlayerSettings p $ V2 0 200

