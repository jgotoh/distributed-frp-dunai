{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module ServerMain
  ( reqGameSettings
  , serverMain
  , white
  )
where

import           Control.Concurrent
import           Data.List
import           Control.Monad.STM
import           Control.Concurrent.STM.TMVar
import qualified Data.Map.Strict               as Map
import           Control.Monad.Trans.MSF.Reader
import           FRP.BearRiver.Network.TimeWarp
import           Network.Common
import           Network.Server
import           Data.IORef
import           GameState
import           Time
import           FRP.BearRiver
import           ProcessExtra
import           FRP.BearRiver.Network.Reactimate
import qualified Control.Distributed.Process   as P
import           Types
import qualified SDL
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import           ServerGame

-- return the GameSettings for a client identified by P.ProcessId
-- this shows how to extend a server's api to handle new requests
-- reqGameSettings is visible to clients and blocks until the result arrives
reqGameSettings
  :: (Addressable a) => a -> P.ProcessId -> P.Process (Maybe GameSettings)
reqGameSettings s pid = MP.call s $ GSRequest pid

-- type to encode requests
newtype GSRequest = GSRequest P.ProcessId
  deriving (Generic, Show, Typeable)
instance Binary GSRequest

-- handler to be added to a server's ProcessDefinition
handleGSRequest :: MP.CallHandler (ServerState a) GSRequest (Maybe GameSettings)
handleGSRequest s (GSRequest pid) =
  MP.reply (gsClient s (gameSettings 0) pid) s

-- return the GameState for a specific client
gsClient :: ServerState a -> GameSettings -> P.ProcessId -> Maybe GameSettings
gsClient s gs pid = case index of
  Nothing -> Nothing
  Just x  -> case x of
    0 -> Just gs
    _ -> Just gs { localPlayerSettings  = remotePlayerSettings gs
                 , remotePlayerSettings = localPlayerSettings gs
                 }
  where index = elemIndex pid (pidClient <$> s)

getConfiguration
  :: LocalNode
  -> HostName
  -> Port
  -> SessionName
  -> ServerConfiguration NetState
getConfiguration node ip port name = defaultServerConfig node ip port name def
 where
  def =
    addApiHandler (MP.handleCall handleGSRequest) defaultFRPProcessDefinition

gameSettings :: Int -> GameSettings
gameSettings roundLength = GameSettings psA psB bs (fromIntegral roundLength)
 where
  psA = PlayerSettings (SDL.V2 50 100) (SDL.V2 10 50) (SDL.V2 0 225) white
  psB = PlayerSettings (SDL.V2 450 100) (SDL.V2 10 50) (SDL.V2 0 225) white
  bs  = BallSettings (SDL.V2 250 175)
                     4
                     (SDL.V2 250 250)
                     (SDL.V4 255 255 0 255)
                     (SDL.V2 (-0.75) $ -0.12)

serverMain :: HostName -> Port -> SessionName -> Int -> Bool -> IO ()
serverMain ip p n roundLength useTimeWarp = do
  Right (node, _)                   <- initializeNode ip p
  s@(LocalServer _ started sQ rQ _) <- startServerProcess
    $ getConfiguration node ip p n

  (Right api) <- atomically $ readTMVar started

  -- SDL is used only to sense time
  SDL.initialize [SDL.InitTimer]

  print "waiting for clients to join"
  [clientA, clientB] <- waitUntilState s (\s' -> length s' == 2)

  print "two clients have joined"
  let portA = serverStateSendPort $ serverStateClient clientA
      portB = serverStateSendPort $ serverStateClient clientB
      pidA  = P.sendPortProcessId $ P.sendPortId portA
      pidB  = P.sendPortProcessId $ P.sendPortId portB
      pids  = Map.fromList [(LocalPlayer, pidA), (RemotePlayer, pidB)]

  -- FPS:
  frameNrRef <- newIORef 0
  startTime  <- createTimeRef

  timeRef    <- createTimeRef
  if not useTimeWarp
    then
    -- reactimate, gather inputs of clients, send UpdatePackets (= snapshots of whole world)
         reactimateServer
      (return undefined) -- equivalent to initial GameInput, not needed here
      (sense timeRef) -- get the DTime
      (actuate frameNrRef)
      (runGameReader (gameSettings roundLength) (serverSF pids))
      (receiveCommand rQ) -- get CommandPackets
      (writeState sQ . createNetStates portA portB api)
    else reactimateTimeWarp
      (return (GameInput Nothing))
      (sense timeRef)
      (actuate frameNrRef)
      (runGameReader (gameSettings roundLength) (serverSFWarp pids frames))
      (receiveCommands rQ)
      (writeState sQ . createNetStatesWithFrame portA portB api)
      frames

  dtTime         <- senseTime startTime
  numberOfFrames <- readIORef frameNrRef

  let dtMs = dtTime
      fps  = fromIntegral numberOfFrames / dtMs

  print $ "FPS: " ++ show fps

  print "round over, server will quit"
  threadDelay 1000000
  SDL.quit
  where frames = 100

actuate :: IORef Integer -> Bool -> GameState -> IO Bool
actuate frameNrRef _ gs = do

  previousFrame <- readIORef frameNrRef
  writeIORef frameNrRef $ previousFrame + 1

  return $ gameOver gs

createNetStates
  :: P.SendPort (UpdatePacket NetState)
  -> P.SendPort (UpdatePacket NetState)
  -> P.ProcessId
  -> GameState
  -> [(P.SendPort (UpdatePacket NetState), UpdatePacket NetState)]
createNetStates !portA !portB server !gs =
  createNetStatesWithFrame portA portB server (0, gs)

createNetStatesWithFrame
  :: P.SendPort (UpdatePacket NetState)
  -> P.SendPort (UpdatePacket NetState)
  -> P.ProcessId
  -> (FrameNr, GameState)
  -> [(P.SendPort (UpdatePacket NetState), UpdatePacket NetState)]
createNetStatesWithFrame !portA !portB server (nr, gs) = (fmap . fmap)
  (UpdatePacket server nr)
  [playerA, playerB]
 where
  playerA = (portA, aState)
  playerB = (portB, bState)
  aState  = NetState (localPlayerState gs)
                     (remotePlayerState gs)
                     (ballState gs)
                     (gameOver gs)
  bState = NetState (remotePlayerState gs)
                    (localPlayerState gs)
                    (ballState gs)
                    (gameOver gs)

sense :: IORef DTime -> Bool -> IO (DTime, Maybe a)
sense timeRef _ = do
  _ <- fixedTimeStep 33.333 timeRef
  -- dtSecs <- senseTime timeRef
  -- print $ "IO dtSecs: " ++ show dtSecs
  -- return (dtSecs, Nothing)
  -- print dtSecs
  return (0.033333, Nothing)

runGameReader :: Monad m => GameSettings -> SF (GameEnv m) a b -> SF m a b
runGameReader gs sf = readerS $ runReaderS_ (runReaderS sf) gs

white :: Color
white = SDL.V4 255 255 255 255

-- orange = SDL.V4 240 142 125 255
-- green = SDL.V4 130 161 59 255

