module Game where

import GameState

import Control.Monad.Reader hiding (asks, ask)
import Control.Monad.Trans.MSF.Reader
import Data.Functor.Identity
import FRP.BearRiver hiding ((^+^))
import SDL.Vect

gameSF :: Monad m => SF m a GameState
gameSF = (constM ask) >>> arr (\dt -> (dt, ())) >>> runReaderS_ (runReaderS_ (runReaderS (movingBall v0)) gs) ps >>> (arr $ GameState)
  where
    v0 = V2 0 50
    gs = GameSettings 100
    ps = PlayerSettings (V2 100 300) (V2 0 200)

gravity :: Double
gravity = -40

fallingBall :: Monad m => Velocity -> SF (PlayerEnv m) () Position
fallingBall v0 = proc () -> do
  dv <- integral -< gravity
  v1 <- arr (\dv -> case v0 of V2 vx vy -> V2 vx (vy+dv)) -< dv
  dp <- integral -< v1
  pos1 <- arrM(\dp -> ((dp) +) <$> (lift $ asks playerPosition0)) -< dp
  returnA -< pos1

hitGround :: Monad m => SF (GameEnv m) Position Bool
hitGround = arrM $ \b -> (yOfV2 b <=) <$> (lift $ asks groundHeightSettings)
  where
    yOfV2 v = case v of V2 _ y -> y

-- Converts sf to stream of events. Events hold values when sf returned true.
edgeSF :: Monad m => SF m a Bool -> SF m a (Event a)
edgeSF sf = (arr id &&& sf) >>> arr (\(x, bool) -> if bool then Event x else NoEvent)

movingBall :: Monad m => Velocity -> SF (GameEnv (PlayerEnv m)) () Position
movingBall v0 = switch ((liftTransSF $ fallingBall v0) >>> (arr id &&& edgeSF (hitGround))) bouncingBall

liftFallingSF :: Monad m => Velocity -> MSF (ClockInfo (GameEnv (PlayerEnv m))) () Position
liftFallingSF v0 = liftTransSF $ fallingBall v0

-- flattens a ReaderT r1 to arrow input, gives rise to an outer Reader r2
liftInner :: Monad m => MSF (ReaderT r1 m) a b -> MSF (ReaderT r2 m) (r1, a) b
liftInner sf = readerS $ addInput $ runReaderS sf

-- creates a ReaderT in an inner layer
liftTransSF :: Monad m => MSF (ReaderT t1 m) a b -> MSF (ReaderT t1 (ReaderT t2 m)) a b
liftTransSF sf = readerS $ liftInner sf

-- send an additional argument into an arrow
addInput :: Monad m => MSF m a b -> MSF m (c,a) b
addInput sf = second sf >>> arr snd

staticBall :: Monad m => Position -> SF (m) () Position
staticBall p0 = constM $ return p0

bouncingBall :: Monad m => Position -> SF (GameEnv (PlayerEnv m)) () Position
bouncingBall _ = movingBall $ V2 0 200

