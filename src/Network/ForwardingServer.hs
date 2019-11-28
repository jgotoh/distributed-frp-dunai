-- a Server that forwards messages to connected clients
module Network.ForwardingServer
  ( clientUpdate
  , joinRequest
  , serverProcessDef
  , startServerProcess
  , ServerProcessDefinition
  )
where

import           Network.Internal.ServerCommon
import           Network.Common

import           Control.Concurrent
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.SystemLog
                                               as Log
-- import qualified Control.Distributed.Process.Extras.Time
                                               -- as Time
import           Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import qualified Control.Distributed.Process.Node
                                               as Node
import           Control.Monad
import qualified Network.Socket                as N
import qualified Network.Transport.TCP         as NT
import qualified Network.Transport             as T

-- client facing API

-- Sends a StateUpdate
clientUpdate
  :: (Addressable a, Binary m, Typeable m) => a -> StateUpdate m -> P.Process ()
clientUpdate = MP.cast

startServerProcess
  :: (Binary a, Typeable a)
  => N.HostName
  -> N.ServiceName
  -> String
  -> ServerProcessDefinition a
  -> IO ()
startServerProcess ip port name def = do
  Right transport <- NT.createTransport (NT.defaultTCPAddr ip port)
                                        NT.defaultTCPParameters
  -- TODO print error, also return Either from launchServer
  node <- createLocalNode transport
  Node.runProcess node $ do

    _ <- Log.systemLog (P.liftIO . print) (return ()) Log.Debug return

    let serverAddress =
          P.nodeAddress $ Node.localNodeId node :: T.EndPointAddress
    pid <- P.spawnLocal $ serverProcess def []

    P.link pid

    P.liftIO
      $  print
      $  "Server starts at: "
      ++ show serverAddress
      ++ " ProcessId: "
      ++ show pid
    P.register name pid
    P.liftIO $ forever $ threadDelay 16
  return ()

serverProcessDef :: (Binary a, Typeable a) => ServerProcessDefinition a
serverProcessDef = MP.defaultProcess
  { MP.apiHandlers            = [ MP.handleCall handleJoinRequest
                                , MP.handleCast handleStateUpdate
                                ]
  , MP.infoHandlers           = [MP.handleInfo handleMonitorNotification]
  , MP.exitHandlers           = []
  , MP.timeoutHandler         = logTimeout
  , MP.shutdownHandler        = logShutdown
  , MP.unhandledMessagePolicy = MP.Terminate
  }


handleStateUpdate
  :: (Binary a, Typeable a) => MP.CastHandler (ServerState a) (StateUpdate a)
handleStateUpdate s m = do
  broadcastUpdate (withoutClient pid s) m
  MP.continue s
 where
  pid = case m of
    StateUpdate pid' _ -> pid'

-- TODO pass in function that decides whether request is accepted
handleJoinRequest
  :: (Binary a, Typeable a)
  => MP.CallHandler
       (ServerState a)
       (JoinRequest a)
       (JoinRequestResult [Nickname])
handleJoinRequest s (JoinRequest nick serverStatePort) = do
  P.liftIO $ print $ "new JoinRequest" ++ show serverStatePort
  _ <- P.monitorPort (serverStateSendPort serverStatePort)
  let s' = client : s
  P.liftIO $ print $ "JoinRequest:: " ++ show s'
  MP.reply (JoinRequestResult $ Right (JoinAccepted nicks)) s'
 where
  client = Client nick serverStatePort
  nicks  = map nameClient s

handleMonitorNotification
  :: MP.ActionHandler (ServerState a) P.PortMonitorNotification
handleMonitorNotification s (P.PortMonitorNotification ref port reason) = do
  P.liftIO
    $  print
    $  "client process died: "
    ++ show port
    ++ " reason: "
    ++ show reason
  P.liftIO $ print $ "new state: " ++ show s'

  P.unmonitor ref
  MP.continue s'
  where s' = withoutClient' port s
