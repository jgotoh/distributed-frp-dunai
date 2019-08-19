module Game where

import GameState

import Control.Monad.Reader hiding (asks, ask)
import Control.Monad.Trans.MSF.Reader
import FRP.BearRiver



gameSF :: Monad m => SF m a GameState
gameSF = (constM ask) >>> arr (\dt -> (dt, ())) >>> flattenFallingBall >>> (arr $ GameState)

gravity :: Double
gravity = -100

fallingBall :: Monad m => SF (GameEnv m) () Position
fallingBall = proc () -> do
  v <- integral -< gravity
  dp <- integral -< v
  pos1 <- arrM(\d -> (d +) <$> (lift $ asks leftBallPos0)) -< dp
  returnA -< pos1

-- first flattens outer ClockInfo Reader with runReaderS, adds DTime to fallingBall's Arrow input
-- then runReaderS_ runs inner GameEnv Reader to pass in the GameSettings
flattenFallingBall :: Monad m => MSF m (DTime, ()) Position
flattenFallingBall = runReaderS_ (runReaderS fallingBall) gs
  where
    gs = GameSettings 100 500 50

hitGround :: Monad m => SF (GameEnv m) Position Bool
hitGround = arrM $ \b -> (b <=) <$> (lift $ asks groundPosition)

