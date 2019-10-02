{-# LANGUAGE DeriveGeneric #-}

module Network.Server where

import Network.Common

import Control.Concurrent
import qualified Control.Distributed.Process as P
import qualified Control.Distributed.Process.Extras.SystemLog as Log
import qualified Control.Distributed.Process.Extras.Time as Time
import Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process.ManagedProcess as MP
import qualified Control.Distributed.Process.Node as Node
import Control.Monad
import GHC.Generics (Generic)
import qualified Network.Socket as N
import qualified Network.Transport.TCP as NT
import qualified Network.Transport as T
import Type.Reflection


data Client = Client Nickname (P.SendPort (StateUpdate Message))
  deriving (Generic, Show, Typeable)

type ServerState = [Client]

launchServer :: N.HostName -> N.ServiceName -> String -> IO ()
launchServer ip port name = do
  Right transport <- NT.createTransport (NT.defaultTCPAddr ip port) NT.defaultTCPParameters
  node <- createLocalNode transport
  Node.runProcess node $ do

    _ <- Log.systemLog (P.liftIO . print) (return ()) Log.Debug return

    let serverAddress = P.nodeAddress $ Node.localNodeId node :: T.EndPointAddress
    pid <- P.spawnLocal pongServerProcess

    P.link pid

    P.liftIO $ print $ "Server starts at: " ++ (show $ serverAddress) ++ " ProcessId: " ++ (show pid)
    P.register name pid
    P.liftIO $ forever $ threadDelay 16
  return ()

pongServerProcess :: P.Process ()
pongServerProcess = MP.serve () initHandler pongProcessDef
  where
    initHandler _ = return (MP.InitOk [] Time.NoDelay)

pongProcessDef :: MP.ProcessDefinition ServerState
pongProcessDef = MP.defaultProcess {
                    MP.apiHandlers = [
                        MP.handleCall_ callPong
                        , MP.handleCall handleJoinRequest
                        , MP.handleCast castPong
                                  ]
                    , MP.infoHandlers = [
                        MP.handleInfo logInfo
                        , MP.handleInfo handleMonitorNotification
                                      ]
                    , MP.exitHandlers = [
                        MP.handleExit logExit
                                        ]
                    , MP.timeoutHandler = logTimeout
                    , MP.shutdownHandler = logShutdown
                    , MP.unhandledMessagePolicy = MP.Log
                    }


-- TODO if decline if client with nickname already exists
handleJoinRequest :: MP.CallHandler ServerState JoinRequest JoinRequestResult
handleJoinRequest s (JoinRequest nick port) = do
  P.liftIO $ print $ "JoinRequest:: " ++ (show s')
  _ <- P.monitorPort port
  MP.reply (JoinRequestResult $ Right JoinAccepted) s'
  where
    client = Client nick port
    s' = client:s

callPong :: Message -> P.Process Message
callPong x = do
  P.liftIO $ print $ "server: " ++ (show x)
  return Pong

castPong :: s -> Message -> MP.Action s
castPong s x = do
  P.liftIO $ print $ "server: " ++ (show x)
  MP.continue s

handleMonitorNotification :: MP.ActionHandler ServerState P.PortMonitorNotification
handleMonitorNotification s (P.PortMonitorNotification ref port reason) = do
  P.liftIO $ print $ "client process died: " ++ (show port) ++ " reason: " ++ (show reason)
  P.liftIO $ print $ "new state: " ++ (show s')
  P.unmonitor ref
  MP.continue s'
  where s' = removeClient port s

removeClient :: P.SendPortId -> ServerState -> ServerState
removeClient portId state = filter (hasPortId portId) state
  where
    hasPortId :: P.SendPortId -> Client -> Bool
    hasPortId id' (Client _ port) = (P.sendPortId port) /= id'

logInfo :: s -> Message -> MP.Action s
logInfo s msg = do
  P.liftIO $ print $ "logInfo: msg: " ++ (show msg)
  MP.continue s

logExit :: P.ProcessId -> s -> Message -> MP.Action s
logExit pid s msg = do
  P.liftIO $ print $ "logExit: " ++ (show pid) ++ " msg: " ++ (show msg)
  MP.continue s

logTimeout :: s -> Time.Delay -> MP.Action s
logTimeout s delay = do
  when (delay /= Time.NoDelay) $ P.liftIO $ print $ "logTimeout: " ++ (show delay)
  MP.continue s

logShutdown :: MP.ExitState s -> ExitReason -> P.Process ()
logShutdown _ reason =
  P.liftIO $ print $ "logShutdown: " ++ (show reason)

