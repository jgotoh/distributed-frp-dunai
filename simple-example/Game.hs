module Game where

import GameState

import Control.Monad.Reader hiding (asks, ask)
import Control.Monad.Trans.MSF.Reader
import FRP.BearRiver hiding ((^+^))
import SDL.Vect



gameSF :: Monad m => SF m a GameState
gameSF = (constM ask) >>> arr (\dt -> (dt, ())) >>> flattenFallingBall >>> (arr $ GameState)

gravity :: Double
gravity = -40

fallingBall :: Monad m => SF (GameEnv m) () Position
fallingBall = proc () -> do
  v <- integral -< gravity
  dp <- integral -< v
  pos1 <- arrM(\d -> ((v2DP d) +) <$> (lift $ asks leftBallSettings)) -< dp
  returnA -< pos1
    where
      v2DP = V2 0

-- first flattens outer ClockInfo Reader with runReaderS, adds DTime to fallingBall's Arrow input
-- then runReaderS_ runs inner GameEnv Reader to pass in the GameSettings
flattenFallingBall :: Monad m => MSF m (DTime, ()) Position
flattenFallingBall = runReaderS_ (runReaderS fallingBall) gs
  where
    gs = GameSettings leftPlayer rightPlayer groundHeight
    leftPlayer = V2 100 300
    rightPlayer = V2 400 300
    groundHeight = 100

hitGround :: Monad m => SF (GameEnv m) Position Bool
hitGround = arrM $ \b -> (yOfV2 b <=) <$> (lift $ asks groundHeightSettings)
  where
    yOfV2 v = case v of V2 _ y -> y
