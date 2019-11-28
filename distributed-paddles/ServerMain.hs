module ServerMain where

import           Control.Monad.STM
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Trans.MSF.Reader
import           Network.Common
import           Network.Server
import           Data.IORef
import           GameState
import           Time
import           FRP.BearRiver
import           FRP.BearRiver.Extra
import qualified SDL
import ServerGame

getConfiguration
  :: LocalNode -> HostName -> Port -> SessionName -> ServerConfiguration Int
getConfiguration node ip port name =
  defaultServerConfig node ip port name defaultFRPProcessDefinition

serverMain :: HostName -> Port -> SessionName -> IO ()
serverMain ip p n = do
  Right (node, _)                      <- initializeNode ip p
  s@(LocalServer pid started sQ rQ st) <- startServerProcess
    $ getConfiguration node ip p n

  (Right _) <- atomically $ readTMVar started

  -- SDL is used only to sense time
  SDL.initialize [SDL.InitTimer]
  timeRef <- createTimeRef

  -- TODO wait until two clients join
  -- waitUntilState s undefined
  -- then start session

  -- reactimate, gather inputs of clients, send cmdpackets (= snapshot of whole world)
  reactimateNet' (return undefined) -- equivalent to initial GameInput, not needed here
                 (sense timeRef) -- get the DTime
                 (\_ _ -> return False) -- actuate: maybe logging or server side rendering
                 (runGameReader gs serverSF)
                 (receiveState rQ) -- gather StateUpdates
                 (writeState id sQ) -- send Snapshots
 where
  gs  = GameSettings psA psB bs
  psA = PlayerSettings (SDL.V2 50 100)
                       (SDL.V2 10 50)
                       (SDL.V2 0 175)
                       orange
  psB = PlayerSettings (SDL.V2 300 100)
                       (SDL.V2 10 50)
                       (SDL.V2 0 175)
                       green
  bs = BallSettings (SDL.V2 200 150)
                      4
                      (SDL.V2 350 350)
                      darkBlue
                      (SDL.V2 (-0.75) $ -0.12)

orange = SDL.V4 240 142 125 255
green = SDL.V4 130 161 59 255
darkBlue = SDL.V4 51 90 161 255

receiveState
  :: Control.Concurrent.STM.TQueue.TQueue (StateUpdate Int) -> IO (Maybe c0)
receiveState = undefined

writeState :: (a1 -> a1) -> TQueue (StateUpdate Int) -> b0 -> IO ()
writeState = undefined

sense :: IORef DTime -> Bool -> IO (DTime, Maybe a)
sense timeRef _ = do
  dtSecs <- senseTime timeRef
  return (dtSecs, Nothing)

runGameReader :: Monad m => GameSettings -> SF (GameEnv m) a b -> SF m a b
runGameReader gs sf = readerS $ runReaderS_ (runReaderS sf) gs
