module Time where

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

