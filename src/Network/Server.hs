
module Network.Server
  ( clientUpdate
  , joinRequest
  , serverProcessDef
  , startServerProcess
  , ServerProcessDefinition
  )
where

import           Network.Internal.ServerCommon
import           Network.Common          hiding ( Client )

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

callPong :: Message -> P.Process Message
callPong x = do
  P.liftIO $ print $ "server: " ++ show x
  return Pong

handleStateUpdate'
  :: (Binary a, Typeable a)
  => MP.ChannelHandler (ServerState a) (StateUpdate a) ()
handleStateUpdate' port state msg = do
  broadcastUpdate (withoutClient' sid state) msg
  MP.continue state
  where sid = P.sendPortId port

logInfo :: s -> Message -> MP.Action s
logInfo s msg = do
  P.liftIO $ print $ "logInfo: msg: " ++ show msg
  MP.continue s

logExit :: P.ProcessId -> s -> Message -> MP.Action s
logExit pid s msg = do
  P.liftIO $ print $ "logExit: " ++ show pid ++ " msg: " ++ show msg
  MP.continue s


