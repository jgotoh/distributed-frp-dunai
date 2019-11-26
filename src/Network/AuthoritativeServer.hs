{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Network.AuthoritativeServer
  ( clientUpdate
  , joinRequest
  , ServerConfiguration(..)
  , ServerProcessDefinition
  , ServerState
  , LocalServer(..)
  , defaultServerConfig
  , startServerProcess
  , defaultFRPProcessDefinition
  , Client(..)
  , HasState(..)
  )
where
import           Data.Binary                    ( Binary )
import           FRP.BearRiver
import           Type.Reflection
import           Network.Common          hiding ( Client )
import           Network.Internal.ServerCommon hiding (handleJoinRequest, handleMonitorNotification)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Catch
import           Control.Monad.State
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import           Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import qualified Control.Distributed.Process.ManagedProcess.Internal.Types
                                               as MP
import qualified Control.Distributed.Process.Node
                                               as Node
-- import           Control.Monad
import qualified Network.Socket                as N
-- import qualified Network.Transport.TCP         as NT

-- client facing API

-- Sends a Command packet

clientUpdate
  :: (Addressable a, Binary m, Typeable m) => a -> StateUpdate m -> P.Process ()
clientUpdate = MP.cast

-- server setup

data LocalServer a = LocalServer {
  pidServer :: P.ProcessId
  , pidApiServer :: TMVar (Either SomeException P.ProcessId)
  , sendQueue :: TQueue (StateUpdate a)
  , readQueue :: TQueue (StateUpdate a)
  , stateServer :: TVar (ServerState a)
  }

class HasState a b | a -> b where
  getState :: a -> IO (ServerState b)

instance HasState (LocalServer a) a where
  getState ser = atomically . readTVar $ stateServer ser

instance Show (LocalServer a) where
  show (LocalServer pid _ _ _ _) = "LocalServer{ pid=" ++ show pid ++ "}"

instance Addressable (LocalServer a)

instance Resolvable (LocalServer a) where
  resolve a = case a of
    LocalServer pid _ _ _ _ -> return $ Just pid
  unresolvableMessage a = "LocalServer could not be resolved: " ++ show a

instance Routable (LocalServer a) where
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
  -> ServerConfiguration a
defaultServerConfig node ip port name def = ServerConfiguration
  { nodeConfig              = node
  , hostConfig              = ip
  , portConfig              = port
  , nameConfig              = name
  , processDefinitionConfig = def
  , joinConfig              =
    \s _ -> return $ JoinRequestResult $ Right $ JoinAccepted $ map nameClient s
  }

defaultFRPProcessDefinition
  :: (Binary a, Typeable a) => ServerProcessDefinition a
defaultFRPProcessDefinition = MP.defaultProcess
  { MP.apiHandlers            = []
  , MP.infoHandlers           = []
  , MP.exitHandlers           = []
  , MP.timeoutHandler         = logTimeout
  , MP.shutdownHandler        = logShutdown
  , MP.unhandledMessagePolicy = MP.Terminate
  }


addApiHandler
  :: MP.Dispatcher s -> MP.ProcessDefinition s -> MP.ProcessDefinition s
addApiHandler h def = def { MP.apiHandlers = h : MP.apiHandlers def }

addExternHandler
  :: MP.ExternDispatcher s -> MP.ProcessDefinition s -> MP.ProcessDefinition s
addExternHandler h def = def { MP.externHandlers = h : MP.externHandlers def }

addInfoHandler
  :: MP.DeferredDispatcher s -> MP.ProcessDefinition s -> MP.ProcessDefinition s
addInfoHandler h def = def { MP.infoHandlers = h : MP.infoHandlers def }

handleMonitorNotification
  :: TVar [Client a]
  -> MP.ActionHandler (ServerState a) P.PortMonitorNotification
handleMonitorNotification v s (P.PortMonitorNotification ref port reason) = do
  P.liftIO
    $  print
    $  "client process died: "
    ++ show port
    ++ " reason: "
    ++ show reason
  P.liftIO $ print $ "new state: " ++ show s'
  P.liftIO $ atomically $ writeTVar v s'
  P.unmonitor ref
  MP.continue s'
  where s' = withoutClient' port s

sendStateProcess
  :: (Binary a, Typeable a, HasState s a)
  => TQueue (StateUpdate a)
  -> s
  -> CommandRate
  -> P.Process ()
sendStateProcess q s r = forever $ delay >> do
  x  <- readQ q
  cs <- P.liftIO $ getState s
  sendState cs x
 where
  readQ q' = P.liftIO . atomically $ do
    xs <- flushTQueue q'
    return xs
  -- TODO sendState currently only sends the newest state. may even be necessary here?
  sendState css (x : _) = broadcastUpdate css x
  sendState _   ([]   ) = return ()
  delay = P.liftIO $ threadDelay (Time.asTimeout r)

-- Starts a server using the supplied config, writes into TMVar when start was successful
startServerProcess
  :: (Binary a, Typeable a) => ServerConfiguration a -> IO (LocalServer a)
startServerProcess cfg = do
  started <- newEmptyTMVarIO
  sQueue  <- newTQueueIO
  rQueue  <- newTQueueIO
  stateV  <- newTVarIO state0
  pid     <- Node.forkProcess node $ catch
    (do
      P.liftIO $ print "process forked"
      let serverAddress = P.nodeAddress $ Node.localNodeId node

      mainPid <- P.getSelfPid

      -- TODO better way to modify ProcessDefinition?
      let
        def =
          addApiHandler
              (MP.handleCall $ handleJoinRequest stateV $ joinConfig cfg)
            $ processDefinitionConfig cfg

      let
        def' =
          addInfoHandler (MP.handleInfo $ handleMonitorNotification stateV) def

      let def'' =
            addApiHandler (MP.handleCast $ handleStateUpdate rQueue) def'

      pid <- P.spawnLocal $ serverProcess def'' state0

      let server = LocalServer mainPid started sQueue rQueue stateV

      outPid <- P.liftIO $ Node.forkProcess
        node
        (sendStateProcess sQueue server (Time.milliSeconds 16))

      P.link pid
      P.link outPid

      P.liftIO
        $  print
        $  "Server starts at: "
        ++ show serverAddress
        ++ " ProcessId: "
        ++ show mainPid
        ++ " Api ProcessId:"
        ++ show pid

      P.register (nameConfig cfg) pid
      P.liftIO . atomically $ putTMVar started (Right pid)
      P.liftIO $ forever $ threadDelay 16
    )
    (\e -> do
      P.liftIO $ print "internal server error"
      _ <- P.liftIO $ atomically $ tryPutTMVar started
                                               (Left (e :: SomeException))
      P.liftIO $ print $ show (e :: SomeException)
      throwM e
    )
  return $ LocalServer pid started sQueue rQueue stateV
 where
  node   = nodeConfig cfg
  state0 = []

handleJoinRequest
  :: (Binary a, Typeable a)
  => TVar [Client a]
  -> (  ServerState a
     -> JoinRequest a
     -> P.Process (JoinRequestResult [Nickname])
     )
  -> MP.CallHandler
       (ServerState a)
       (JoinRequest a)
       (JoinRequestResult [Nickname])
handleJoinRequest stateV resultP s req@(JoinRequest nick port) = do
  P.liftIO $ print $ "new JoinRequest: " ++ show req
  result <- resultP s req
  case result of
    JoinRequestResult x -> case x of
      Left _ -> do
        P.liftIO $ print $ "Declined, state: " ++ show s
        MP.reply result s
      Right _ -> do
        let s' = client : s
        _ <- P.monitorPort (serverStateSendPort port)
        P.liftIO $ print $ "Accepted, state: " ++ show s'
        P.liftIO $ atomically $ writeTVar stateV s'
        MP.reply result s'
  where client = Client nick port

handleStateUpdate
  :: (Binary a, Typeable a)
  => TQueue (StateUpdate a)
  -> MP.CastHandler (ServerState a) (StateUpdate a)
handleStateUpdate q s m@(StateUpdate pid msg) = do
  when (pid `elem` (ids <$> s)) (writeQ m)
  MP.continue s
  where
    writeQ = P.liftIO . atomically . writeTQueue q
    ids = (P.sendPortProcessId . P.sendPortId . serverStateSendPort . serverStateClient)

