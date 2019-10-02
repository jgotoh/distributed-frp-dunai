{-# LANGUAGE TupleSections #-}

module Game where

import GameState
import Input

import Control.Monad.Reader (lift)
import Control.Monad.Trans.MSF.Reader
import FRP.BearRiver
import SDL.Vect hiding (identity)

gameSF'' :: Monad m => SF m a GameState
gameSF'' = constM ask >>> arr (, ())
  >>> runReaderS_ (runReaderS_ (runReaderS bouncingBall) gs) ps >>> arr GameState
    where
      gs = GameSettings 50
      ps = PlayerSettings (V2 100 75) (V2 0 100)

gameSF' :: Monad m => SF m a GameState
gameSF' = arr $ \_ -> GameState $ V2 100 100

gameSF :: Monad m => SF m GameInput GameState
gameSF = constM ask &&& identity
  >>> runReaderS_ (runReaderS_ (runReaderS jumpingBall) gs) ps >>> arr GameState
    where
      gs = GameSettings 50
      ps = PlayerSettings (V2 100 75) (V2 0 100)

gravity :: Velocity
gravity = V2 0 (-150)

jumpingBall :: Monad m => SF (GameEnv (PlayerEnv m)) GameInput Position
jumpingBall = switch (morphS (mapReaderT lift) movingBall >>> (arr id &&& edgeSF hitGround)) waitForJump

waitForJump :: Monad m => Position -> SF (GameEnv (PlayerEnv m)) GameInput Position
waitForJump p = switch (arr id &&& constBall p >>> jumpedEventSF)
  (\_ -> morphS (mapReaderT $ mapReaderT $ withReaderT ps) jumpingBall)
    where
      ps _ = PlayerSettings p $ V2 0 150

jumpedEventSF :: Monad m => SF (GameEnv (PlayerEnv m)) (GameInput, Position) (Position, Event ())
jumpedEventSF = arr (\(gi, p) -> (p, jumpInput gi)) >>> second edge

movingBall :: Monad m => SF (PlayerEnv m) a Position
movingBall = proc _ -> do
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
bouncingBall :: Monad m => SF (GameEnv (PlayerEnv m)) a Position
bouncingBall = switch (morphS (mapReaderT lift) movingBall >>> (arr id &&& edgeSF hitGround)) bounceAgain

-- Initialize bouncingBall at position p with another velocity
bounceAgain :: Monad m => Position -> SF (GameEnv (PlayerEnv m)) a Position
bounceAgain p = morphS (mapReaderT $ mapReaderT $ withReaderT ps) bouncingBall
  where
    ps _ = PlayerSettings p $ V2 0 200

-- Const position p0
constBall :: Monad m => Position -> SF m a Position
constBall p0 = constM $ return p0

