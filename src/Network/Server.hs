{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Network.Server
  ( clientUpdate
  , joinRequest
  , snapshot
  , ServerConfiguration(..)
  , ServerProcessDefinition
  , ServerState
  , LocalServer(..)
  , defaultServerConfig
  , startServerProcess
  , defaultFRPProcessDefinition
  , Client(..)
  , HasState(..)
  , waitUntilState
  , Resolvable(..)
  , module Control.Distributed.Process.Extras
  , addApiHandler
  , Network.Internal.ServerCommon.pidClient
  )
where
import           Data.Binary                    ( Binary )
import           Type.Reflection
import           Network.Common
import           Network.Internal.ServerCommon
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Catch
import           Control.Monad.State
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import           Control.Distributed.Process.Extras
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import qualified Control.Distributed.Process.Node
                                               as Node
-- import           Control.Monad
import qualified Network.Socket                as N
-- import qualified Network.Transport.TCP         as NT

-- client facing API

-- Sends a Command packet
clientUpdate
  :: (Addressable a, Binary m, Typeable m)
  => a
  -> CommandPacket m
  -> P.Process ()
clientUpdate = MP.cast

-- TODO implement to avoid hardcoded values in distributed-paddles
-- Requests a snapshot (current state) of the world
snapshot
  :: (Addressable a, Binary m, Typeable m)
  => a
  -> P.ProcessId
  -> P.Process (UpdatePacket m)
snapshot = MP.call

-- server setup

data LocalServer a b = LocalServer {
  pidServer :: P.ProcessId
  , pidApiServer :: TMVar (Either SomeException P.ProcessId)
  , sendQueue :: TQueue ([(P.SendPort (UpdatePacket b), UpdatePacket b)])
  , readQueue :: TQueue (CommandPacket a)
  , stateServer :: TVar (ServerState b)
  }

class HasState a b | a -> b where
  getState :: a -> IO (ServerState b)
  getStateSTM :: a -> STM (ServerState b)

instance HasState (LocalServer a b) b where
  getState ser = readTVarIO $ stateServer ser
  getStateSTM ser = readTVar $ stateServer ser

instance Show (LocalServer a b) where
  show (LocalServer pid _ _ _ _) = "LocalServer{ pid=" ++ show pid ++ "}"

instance Addressable (LocalServer a b)

instance Resolvable (LocalServer a b) where
  resolve a = case a of
    LocalServer pid _ _ _ _ -> return $ Just pid
  unresolvableMessage a = "LocalServer could not be resolved: " ++ show a

instance Routable (LocalServer a b) where
  sendTo s m = resolve s >>= maybe (error $ unresolvableMessage s) (`P.send` m)
  unsafeSendTo s m =
    resolve s >>= maybe (error $ unresolvableMessage s) (`P.unsafeSend` m)

defaultServerConfig
  :: (Binary a, Typeable a)
  => Node.LocalNode
  -> N.HostName
  -> Port
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
  => TQueue ([(P.SendPort (UpdatePacket a), UpdatePacket a)])
  -> s
  -> CommandRate
  -> P.Process ()
sendStateProcess q s r = forever $ delay >> do
  x <- readQ q
  sendStates x
 where
  readQ q' = P.liftIO . atomically $ flushTQueue q'
  -- TODO replace Queue with TMVar. reactimateNet just needs to replace the current value
  delay = P.liftIO $ threadDelay (Time.asTimeout r)

sendStates
  :: (Binary a, Typeable a)
  => [[(P.SendPort (UpdatePacket a), UpdatePacket a)]]
  -> P.Process ()
sendStates msgs = forM_ msgs (\xs -> forM_ xs (uncurry P.sendChan))

-- Blocks until the state satisfies a certain condition
waitUntilState
  :: (HasState s a) => s -> (ServerState a -> Bool) -> IO (ServerState a)
waitUntilState hs f = atomically $ do
  s <- getStateSTM hs
  when (not $ f s) retry
  return s

-- Starts a server using the supplied config, writes into TMVar when start was successful
startServerProcess
  :: (Binary a, Typeable a, Binary b, Typeable b)
  => ServerConfiguration b
  -> IO (LocalServer a b)
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
            addApiHandler (MP.handleCast $ handleCommandPacket rQueue) def'

      pid <- P.spawnLocal $ serverProcess def'' state0

      let server = LocalServer mainPid started sQueue rQueue stateV

      outPid <- P.liftIO $ Node.forkProcess
        node
        (sendStateProcess sQueue server (Time.milliSeconds 50)) --TODO pass in frequency via config

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
        -- appends client to end of state
        let s' = s ++ [client]
        _ <- P.monitorPort (serverStateSendPort port)
        P.liftIO $ print $ "Accepted, state: " ++ show s'
        P.liftIO $ atomically $ writeTVar stateV s'
        MP.reply result s'
  where client = Client nick port

handleCommandPacket
  :: (Binary a, Typeable a, Binary b, Typeable b)
  => TQueue (CommandPacket a)
  -> MP.CastHandler (ServerState b) (CommandPacket a)
handleCommandPacket q s m@(CommandPacket pid _) = do
  when (pid `elem` (ids <$> s)) (writeQ m)
  MP.continue s
 where
  writeQ = P.liftIO . atomically . writeTQueue q
  ids =
    P.sendPortProcessId . P.sendPortId . serverStateSendPort . serverStateClient

