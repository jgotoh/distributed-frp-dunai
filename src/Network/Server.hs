{-# LANGUAGE DeriveGeneric #-}

module Network.Server
  (
  clientUpdate
  , joinRequest
  , serverProcessDef
  , startServerProcess
  , ServerProcessDefinition
  )
where

import           Network.Common          hiding ( Client )

import           Control.Concurrent
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.SystemLog
                                               as Log
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import           Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import qualified Control.Distributed.Process.Node
                                               as Node
import           Control.Monad
import           Data.Binary
import           GHC.Generics                   ( Generic )
import qualified Network.Socket                as N
import qualified Network.Transport.TCP         as NT
import qualified Network.Transport             as T
import           Type.Reflection

-- client facing API

-- Sends a StateUpdate
clientUpdate
  :: (Addressable a, Binary m, Typeable m) => a -> StateUpdate m -> P.Process ()
clientUpdate = MP.cast

-- Sends a JoinRequest and returns the result
joinRequest
  :: (Addressable a, Binary m, Typeable m)
  => a
  -> JoinRequest m
  -> P.Process (JoinRequestResult [Nickname])
joinRequest = MP.call

type ServerProcessDefinition a = MP.ProcessDefinition (ServerState a)

data Client a = Client { nameClient :: Nickname
                       , serverStateClient :: ServerStateSendPort a
                       }
  deriving (Generic, Typeable)

instance Show (Client a) where
  show c = "Client " ++ nameClient c ++ "," ++ show (serverStateClient c)

type ServerState a = [Client a]

startServerProcess :: (Binary a, Typeable a) => N.HostName -> N.ServiceName -> String -> ServerProcessDefinition a -> IO ()
startServerProcess ip port name def = do
  Right transport <- NT.createTransport (NT.defaultTCPAddr ip port)
                                        NT.defaultTCPParameters
  -- TODO print error, also return Either from launchServer
  node <- createLocalNode transport
  Node.runProcess node $ do

    _ <- Log.systemLog (P.liftIO . print) (return ()) Log.Debug return

    let serverAddress =
          P.nodeAddress $ Node.localNodeId node :: T.EndPointAddress
    pid <- P.spawnLocal $ serverProcess def

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

serverProcess :: (Binary a, Typeable a) => ServerProcessDefinition a -> P.Process ()
serverProcess def = MP.serve
  ()
  initHandler
  def
  where initHandler _ = return (MP.InitOk [] Time.NoDelay)

serverProcessDef :: (Binary a, Typeable a) => ServerProcessDefinition a
serverProcessDef = MP.defaultProcess
  { MP.apiHandlers            = [ MP.handleCall_ callPong
                                , MP.handleCall handleJoinRequest
                                , MP.handleRpcChan handleStateUpdate'
                                , MP.handleCast handleStateUpdate
                                ]
  , MP.infoHandlers           = [ MP.handleInfo logInfo
                                , MP.handleInfo handleMonitorNotification
                                ]
  , MP.exitHandlers           = [MP.handleExit logExit]
  , MP.timeoutHandler         = logTimeout
  , MP.shutdownHandler        = logShutdown
  , MP.unhandledMessagePolicy = MP.Log
  }

-- TODO pass in function that decides whether request is accepted
-- TODO if decline if client with nickname already exists
handleJoinRequest
  :: (Binary a, Typeable a)
  => MP.CallHandler
       (ServerState a)
       (JoinRequest a)
       (JoinRequestResult [Nickname])
handleJoinRequest s (JoinRequest nick serverStatePort) = do
  _ <- P.monitorPort (serverStateSendPort serverStatePort)
  let s' = client : s
  P.liftIO $ print $ "JoinRequest:: " ++ show s'
  MP.reply (JoinRequestResult $ Right (JoinAccepted nicks)) s'
 where
  client = Client nick serverStatePort
  nicks  = map nameClient s

handleStateUpdate
  :: (Binary a, Typeable a) => MP.CastHandler (ServerState a) (StateUpdate a)
handleStateUpdate s m = do
  broadcastUpdate m (withoutClient pid s)
  MP.continue s
 where
  pid = case m of
    StateUpdate pid' _ -> pid'

callPong :: Message -> P.Process Message
callPong x = do
  P.liftIO $ print $ "server: " ++ show x
  return Pong

handleStateUpdate'
  :: (Binary a, Typeable a)
  => MP.ChannelHandler (ServerState a) (StateUpdate a) ()
handleStateUpdate' port state msg = do
  broadcastUpdate msg (withoutClient' sid state)
  MP.continue state
  where sid = P.sendPortId port

broadcastUpdate
  :: (Binary a, Typeable a) => StateUpdate a -> [Client a] -> P.Process ()
broadcastUpdate msg clients = forM_ clients (serverUpdate msg)

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

serverUpdate
  :: (Binary a, Typeable a) => StateUpdate a -> Client a -> P.Process ()
serverUpdate m c = P.sendChan (serverStateSendPort (serverStateClient c)) m

hasProcessId :: P.ProcessId -> Client a -> Bool
hasProcessId id' (Client _ port) =
  P.sendPortProcessId (P.sendPortId (serverStateSendPort port)) == id'

-- TODO summarize withoutClient[']. maybe via contramap
withoutClient' :: P.SendPortId -> ServerState a -> ServerState a
withoutClient' portId =
  filter (not <$> hasProcessId (P.sendPortProcessId portId))

withoutClient :: P.ProcessId -> ServerState a -> ServerState a
withoutClient pid = filter (not <$> hasIdentification pid)

generalizedHasId :: Eq b => b -> (Client a -> b) -> Client a -> Bool
generalizedHasId b f c = b == f c

hasIdentification :: P.ProcessId -> Client a -> Bool
hasIdentification pid c = pid == P.sendPortProcessId
  (P.sendPortId $ serverStateSendPort $ serverStateClient c)

logInfo :: s -> Message -> MP.Action s
logInfo s msg = do
  P.liftIO $ print $ "logInfo: msg: " ++ show msg
  MP.continue s

logExit :: P.ProcessId -> s -> Message -> MP.Action s
logExit pid s msg = do
  P.liftIO $ print $ "logExit: " ++ show pid ++ " msg: " ++ show msg
  MP.continue s

logTimeout :: s -> Time.Delay -> MP.Action s
logTimeout s delay = do
  when (delay /= Time.NoDelay) $ P.liftIO $ print $ "logTimeout: " ++ show delay
  MP.continue s

logShutdown :: MP.ExitState s -> ExitReason -> P.Process ()
logShutdown _ reason = P.liftIO $ print $ "logShutdown: " ++ show reason

