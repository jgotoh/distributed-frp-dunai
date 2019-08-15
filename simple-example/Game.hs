module Game where

import FRP.BearRiver

import Control.Monad.Reader hiding (asks, ask)
import Control.Monad.Trans.MSF.Reader


type GameEnv = ReaderT GameSettings

data GameSettings = GameSettings
  { leftBallPos :: Position
  , rightBallPos :: Position
  , groundPosition :: Position
  }


type Ball = Double
type Position = Double

gameSF :: Monad m => SF m a ()
gameSF = (constM ask) >>> arr (\dt -> (dt, ())) >>> flattenFallingBall >>> (arr $ return ())

gravity :: Double
gravity = -100

fallingBall :: Monad m => SF (GameEnv m) () Ball
fallingBall = proc () -> do
  v <- integral -< gravity
  dp <- integral -< v
  pos1 <- arrM(\d -> (d +) <$> (lift $ asks leftBallPos)) -< dp
  returnA -< pos1

-- first flattens outer ClockInfo Reader with runReaderS, adds DTime to fallingBall's Arrow input
-- then runReaderS_ runs inner GameEnv Reader and passes in the GameSettings
flattenFallingBall :: Monad m => MSF m (DTime, ()) Position
flattenFallingBall = runReaderS_ (runReaderS fallingBall) gs
  where
    gs = GameSettings 100 500 50

hitGround :: Monad m => SF (GameEnv m) Ball Bool
hitGround = arrM $ \b -> (b <=) <$> (lift $ asks groundPosition)

