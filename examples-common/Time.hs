module Time where

import           Control.Concurrent
import           Data.IORef
import           FRP.BearRiver
import           SDL.Raw.Timer                 as SDL

createTimeRef :: IO (IORef DTime)
createTimeRef = SDL.getPerformanceCounter >>= \pc -> newIORef $ fromIntegral pc

senseTime :: IORef DTime -> IO DTime
senseTime timeRef = do
  newTime      <- SDL.getPerformanceCounter
  freq         <- SDL.getPerformanceFrequency
  previousTime <- readIORef timeRef

  writeIORef timeRef $ fromIntegral newTime
  let dtSecs = (fromIntegral newTime - previousTime) / fromIntegral freq
  return dtSecs

-- Implements a fixed time step of x ms. Blocks until x ms have elapsed
fixedTimeStep :: Double -> IORef DTime -> IO DTime
fixedTimeStep millis timeRef = do
  newTime      <- SDL.getPerformanceCounter
  freq         <- SDL.getPerformanceFrequency
  previousTime <- readIORef timeRef

  let dtSecs = (fromIntegral newTime - previousTime) / fromIntegral freq

  -- last frame took dtSecs to compute
  let block  = truncate ((millisToMicros (millis)) - (secondsToMicros dtSecs))

  -- blocking time is < 0 if the last frame took too long to compute
  -- we now have to block until the next frame
  -- see Jason Gregory, Game Engine Architecture, 2nd. Edition, p.351: Governing the Frame Rate
  if block < 0
    then do
      threadDelay $ truncate $ millisToMicros millis + fromIntegral block
    else do
      threadDelay $ block

  -- new dtSecs after threadDelay
  newTime' <- SDL.getPerformanceCounter
  let dtSecs' = (fromIntegral newTime' - previousTime) / fromIntegral freq

  writeIORef timeRef $ fromIntegral newTime'
  return dtSecs'
 where
  secondsToMicros s = s * 1000 * 1000
  millisToMicros ms = ms * 1000

