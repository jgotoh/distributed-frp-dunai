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

hitGround :: Monad m => SF (GameEnv m) Position Bool
hitGround = arrM $ \b -> (yOfV2 b <=) <$> lift (asks groundHeightSettings)
  where
    yOfV2 v = case v of V2 _ y -> y

-- Converts sf to stream of events. Events hold values when sf returned true.
edgeSF :: Monad m => SF m a Bool -> SF m a (Event a)
edgeSF sf = (arr id &&& sf) >>> arr (\(x, bool) -> if bool then Event x else NoEvent)

-- A ball that bounces back up when it hit ground
bouncingBall :: Monad m => SF (GameEnv (PlayerEnv m)) () Position
bouncingBall = switch (morphS (mapReaderT lift) movingBall >>> (arr id &&& edgeSF hitGround)) bounceAgain

-- Initialize bouncingBall at position p with another velocity
bounceAgain :: Monad m => Position -> SF (GameEnv (PlayerEnv m)) () Position
bounceAgain p = morphS (mapReaderT $ mapReaderT $ withReaderT ps) bouncingBall
  where
    ps _ = PlayerSettings p $ V2 0 200

-- Const position p0
constBall :: Monad m => Position -> SF m () Position
constBall p0 = constM $ return p0

