{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module ServerMain
  ( reqGameSettings
  , serverMain
  )
where

import           Control.Concurrent
import           Data.List
import           Control.Monad.STM
import           Control.Monad
import           Control.Applicative
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TQueue
import qualified Data.Map.Strict               as Map
import           Control.Monad.Trans.MSF.Reader
import           Network.Common
import           Network.Server
import           Data.IORef
import           GameState
import           Time
import           FRP.BearRiver
import           FRP.BearRiver.Extra
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
handleGSRequest s (GSRequest pid) = MP.reply (gsClient s gameSettings pid) s

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

gameSettings :: GameSettings
gameSettings = GameSettings psA psB bs
 where
  psA = PlayerSettings (SDL.V2 50 100) (SDL.V2 10 50) (SDL.V2 0 175) orange
  psB = PlayerSettings (SDL.V2 300 100) (SDL.V2 10 50) (SDL.V2 0 175) green
  bs  = BallSettings (SDL.V2 200 150)
                     4
                     (SDL.V2 350 350)
                     orange
                     (SDL.V2 (-0.75) $ -0.12)

serverMain :: HostName -> Port -> SessionName -> IO ()
serverMain ip p n = do
  Right (node, _)                   <- initializeNode ip p
  s@(LocalServer _ started sQ rQ _) <- startServerProcess
    $ getConfiguration node ip p n

  (Right api) <- atomically $ readTMVar started

  -- SDL is used only to sense time
  SDL.initialize [SDL.InitTimer]
  timeRef <- createTimeRef

  print "waiting for clients to join"
  [clientA, clientB] <- waitUntilState s (\s' -> (length s') == 2)

  print "two clients have joined"
  let portA = serverStateSendPort $ serverStateClient clientA
      portB = serverStateSendPort $ serverStateClient clientB
      pidA  = P.sendPortProcessId $ P.sendPortId portA
      pidB  = P.sendPortProcessId $ P.sendPortId portB
      pids  = Map.fromList [(LocalPlayer, pidA), (RemotePlayer, pidB)]

  -- reactimate, gather inputs of clients, send UpdatePackets (= snapshots of whole world)
  reactimateServer
    (return undefined) -- equivalent to initial GameInput, not needed here
    (sense timeRef) -- get the DTime
    (\_ _ -> do
      return False
    ) -- actuate: maybe logging or server side rendering
    (runGameReader gameSettings (serverSF pids))
    (receiveState rQ) -- get CommandPackets
    (writeState (createNetStates portA portB api) sQ) -- create UpdatePackets

  SDL.quit

-- TODO replace TQueue with TMVar (NonEmpty Command) to avoid need to flush
receiveState
  :: Control.Concurrent.STM.TQueue.TQueue (CommandPacket Command)
  -> IO (Maybe [CommandPacket Command])
receiveState q = flushQ q >>= \xs -> do
  -- print xs
  toMaybe xs
 where
  flushQ = atomically . flushTQueue
  toMaybe xs = case xs of
    [] -> return Nothing
    _  -> return $ Just xs

createNetStates
  :: P.SendPort (UpdatePacket NetState)
  -> P.SendPort (UpdatePacket NetState)
  -> P.ProcessId
  -> GameState
  -> [(P.SendPort (UpdatePacket NetState), UpdatePacket NetState)]
createNetStates !portA !portB server !gs = (fmap . fmap)
  (UpdatePacket server)
  [playerA, playerB]
  -- [playerA, playerB, playerB, playerB, playerB, playerB, playerB, playerB, playerB, playerB, playerB, playerB, playerB, playerB, playerB]

 where
  playerA = (portA, aState)
  playerB = (portB, bState)
  aState  = NetState (localPlayerState gs) (remotePlayerState gs) (ballState gs)
  bState  = NetState (remotePlayerState gs) (localPlayerState gs) (ballState gs)

writeState
  :: (  GameState
     -> [(P.SendPort (UpdatePacket NetState), UpdatePacket NetState)]
     )
  -> TMVar [(P.SendPort (UpdatePacket NetState), UpdatePacket NetState)]
  -> GameState
  -> IO ()
writeState f q gs = do
  let out = f gs
  atomically . replaceTMVar q $ out

-- Empties a TMVar, then writes a new value
replaceTMVar :: TMVar a -> a -> STM ()
replaceTMVar v a = do
  empty <- isEmptyTMVar v
  if empty
    then putTMVar v a
    else do
      _ <- takeTMVar v
      putTMVar v a

sense :: IORef DTime -> Bool -> IO (DTime, Maybe a)
sense timeRef _ = do
  dtSecs <- fixedTimeStep 16.6 timeRef
  -- dtSecs <- senseTime timeRef
  -- print $ "IO dtSecs: " ++ show dtSecs
  return (dtSecs, Nothing)

runGameReader :: Monad m => GameSettings -> SF (GameEnv m) a b -> SF m a b
runGameReader gs sf = readerS $ runReaderS_ (runReaderS sf) gs

orange :: Color
orange = SDL.V4 240 142 125 255

green :: Color
green = SDL.V4 130 161 59 255

darkBlue :: Color
darkBlue = SDL.V4 51 90 161 255

