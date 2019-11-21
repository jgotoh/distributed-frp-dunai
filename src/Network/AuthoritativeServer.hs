module Network.AuthoritativeServer
  ( clientUpdate
  , joinRequest
  , ServerConfiguration(..)
  , ServerProcessDefinition
  , ServerState
  , LocalServer(..)
  , defaultServerConfig
  , startServerProcess
  , startServerWithClients
  , defaultFRPServerDefinition
  , Client(..)
  )
where
import           Data.Binary                    ( Binary )
import           Type.Reflection
import           Network.Common          hiding ( Client )
import           Network.Internal.ServerCommon
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Catch
import           Control.Monad.State
import qualified Control.Distributed.Process   as P
-- import qualified Control.Distributed.Process.Extras.Time
                                               -- as Time
import           Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import qualified Control.Distributed.Process.Node
                                               as Node
-- import           Control.Monad
import qualified Network.Socket                as N
import qualified Network.Transport.TCP         as NT

-- client facing API

-- Sends a Command packet

clientUpdate
  :: (Addressable a, Binary m, Typeable m)
  => a
  -> StateUpdate [m]
  -> P.Process ()
clientUpdate = MP.cast

-- server setup

data LocalServer = LocalServer {
  pidServer :: P.ProcessId
  , pidApiServer :: TMVar (Either SomeException P.ProcessId)
  }

instance Show LocalServer where
  show (LocalServer pid _) = "LocalServer{pid=}" ++ show pid

instance Addressable LocalServer

instance Resolvable LocalServer where
  resolve a = case a of
    LocalServer pid _ -> return $ Just pid
  unresolvableMessage a = "LocalServer could not be resolved: " ++ show a

instance Routable LocalServer where
  sendTo s m = resolve s >>= maybe (error $ unresolvableMessage s) (`P.send` m)
  unsafeSendTo s m =
    resolve s >>= maybe (error $ unresolvableMessage s) (`P.unsafeSend` m)

defaultServerConfig
  :: (Binary a, Typeable a)
  => Node.LocalNode
  -> N.HostName
  -> N.ServiceName
  -> SessionName
  -> ServerProcessDefinition a
  -> ServerConfiguration IO a
defaultServerConfig node ip port name def = ServerConfiguration
  { nodeConfig              = node
  , hostConfig              = ip
  , portConfig              = port
  , nameConfig              = name
  , transportConfig         = createTCPTransport
  , processDefinitionConfig = def
  , joinConfig              =
    \s _ -> return $ JoinRequestResult $ Right $ JoinAccepted $ map nameClient s
  }
 where
  createTCPTransport =
    NT.createTransport (NT.defaultTCPAddr ip port) NT.defaultTCPParameters

defaultFRPServerDefinition
  :: (Binary a, Typeable a) => ServerProcessDefinition a
defaultFRPServerDefinition = MP.defaultProcess
  { MP.apiHandlers            = [ MP.handleCall handleJoinRequest
                                , MP.handleCast handleStateUpdate
                                ]
  , MP.infoHandlers           = [MP.handleInfo handleMonitorNotification]
  , MP.exitHandlers           = []
  , MP.timeoutHandler         = logTimeout
  , MP.shutdownHandler        = logShutdown
  , MP.unhandledMessagePolicy = MP.Log
  }

-- Starts a server using the supplied config, writes into TMVar when start was successful
startServerProcess
  :: (Binary a, Typeable a) => ServerConfiguration IO a -> IO LocalServer
startServerProcess cfg = do
  v   <- newEmptyTMVarIO
  pid <- Node.forkProcess node $ catch
    (do
      P.liftIO $ print "process forked"
      let serverAddress = P.nodeAddress $ Node.localNodeId node
      pid <- P.spawnLocal $ serverProcess (processDefinitionConfig cfg) []
      P.link pid

      mainPid <- P.getSelfPid

      P.liftIO
        $  print
        $  "Server starts at: "
        ++ show serverAddress
        ++ " ProcessId: "
        ++ show mainPid
        ++ " Api ProcessId:"
        ++ show pid

      P.register (nameConfig cfg) pid
      P.liftIO . atomically $ putTMVar v (Right pid)
      P.liftIO $ forever $ threadDelay 16
    )
    (\e -> do
      P.liftIO $ print "internal server error"
      _ <- P.liftIO $ atomically $ tryPutTMVar v (Left (e :: SomeException))
      P.liftIO $ print $ show (e :: SomeException)
      throwM e
    )
  return $ LocalServer pid v
  where node = nodeConfig cfg

-- Starts the server and manually executes the given JoinRequests.
-- Necessary for unit tests, because SendPorts sent via joinRequest somehow immediately die after calls to handleJoinRequest.
startServerWithClients
  :: (Binary a, Typeable a)
  => ServerConfiguration IO a
  -> P.Process [JoinRequest a]
  -> IO (LocalServer, TMVar [JoinRequestResult [Nickname]])
startServerWithClients cfg reqsP = do
  v     <- newEmptyTMVarIO
  joins <- newEmptyTMVarIO
  pid   <- Node.forkProcess node $ catch
    (do
      P.liftIO $ print "process forked"

      let serverAddress = P.nodeAddress $ Node.localNodeId node
      reqs <- reqsP

      (results, sstate) <- runStateT (evalJoinRequests (joinConfig cfg) reqs) []

      pid <- P.spawnLocal $ serverProcess (processDefinitionConfig cfg) (sstate)

      P.link pid

      mainPid <- P.getSelfPid

      P.liftIO
        $  print
        $  "Server starts at: "
        ++ show serverAddress
        ++ " ProcessId: "
        ++ show mainPid
        ++ " Api ProcessId:"
        ++ show pid

      P.register (nameConfig cfg) pid
      P.liftIO . atomically $ putTMVar v (Right pid)
      P.liftIO . atomically $ putTMVar joins results
      P.liftIO $ forever $ threadDelay 16
    )
    (\e -> do
      -- TODO cloud haskell's exit mechanism rely on exceptions, so this will catch even regular shutdowns
      P.liftIO $ print "internal server error"
      _ <- P.liftIO $ atomically $ tryPutTMVar v (Left (e :: SomeException))
      P.liftIO $ print $ show (e :: SomeException)
      throwM e
    )
  return (LocalServer pid v, joins)
  where node = nodeConfig cfg

evalJoinRequests
  :: (Binary a, Typeable a)
  => (  ServerState a
     -> JoinRequest a
     -> P.Process (JoinRequestResult [Nickname])
     )
  -> [JoinRequest a]
  -> StateT (ServerState a) P.Process ([JoinRequestResult [Nickname]])
evalJoinRequests f rs = case rs of
  []                           -> return []
  r@(JoinRequest nick p) : rs' -> do
    _   <- lift $ P.monitorPort (serverStateSendPort p)
    s   <- get

    res <- lift $ f s r

    case res of
      JoinRequestResult eRes -> case eRes of
        Left _ -> do
          results <- evalJoinRequests f rs'
          return $ res : results
        Right _ -> do
          put $ (Client nick p) : s
          results <- evalJoinRequests f rs'
          return $ res : results

handleStateUpdate
  :: (Binary a, Typeable a) => MP.CastHandler (ServerState a) (StateUpdate [a])
handleStateUpdate s m = do
  undefined --do
  -- broadcastUpdate m (withoutClient pid s)
  -- MP.continue s
 -- where
  -- pid = case m of
    -- StateUpdate pid' _ -> pid'

