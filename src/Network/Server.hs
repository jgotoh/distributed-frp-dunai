{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Server
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
  , waitUntilState
  , Resolvable(..)
  , module Control.Distributed.Process.Extras
  , addApiHandler
  , Network.Internal.ServerCommon.pidClient
  )
where
import           Data.Binary                    ( Binary )
-- import           Data.IORef
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
-- import           Data.Time
-- import qualified Network.Transport.TCP         as NT

-- client facing API --

-- Sends a Command packet
clientUpdate
  :: (Addressable a, Binary m, Typeable m)
  => a
  -> CommandPacket m
  -> P.Process ()
clientUpdate = MP.cast

-- server setup --

data LocalServer a b = LocalServer {
  pidServer :: P.ProcessId
  , pidApiServer :: TMVar (Either SomeException P.ProcessId)
  , sendQueue :: TMVar ([(P.SendPort (UpdatePacket b), UpdatePacket b)])
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

-- default ServerConfiguration that accepts every JoinRequest.
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

-- default ProcessDefinition that terminates on unhandled messages, logs timeouts and shutdowns.
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

-- Starts a server using the supplied config, writes into TMVar when start was successful
startServerProcess
  :: forall a b
   . (Binary a, Typeable a, Binary b, Typeable b)
  => ServerConfiguration b
  -> IO (LocalServer a b)
startServerProcess cfg = do
  started <- newEmptyTMVarIO
  sendVar <- newEmptyTMVarIO
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

      -- let server = LocalServer mainPid started sendVar rQueue stateV

      -- TODO create/ destroy Updaters dynamically when clients join/ quit
      updaters <- (fmap . fmap . fmap) fst spawnUpdateProcesses 2

      outPid   <- P.spawnLocal
        (sendStateProcessSTM updaters sendVar (Time.milliSeconds 50))

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
      -- localNodeId <- Node.localNodeId node
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
  return $ LocalServer pid started sendVar rQueue stateV
 where
  node   = nodeConfig cfg
  state0 = []


-- Sending of Updates --

-- Process that periodically distributes messages read from v to UpdateProcesses.
-- TODO sense time and block for frequency - dt!
sendStateProcessSTM
  :: (Binary b, Typeable b)
  => [TMVar (P.SendPort (UpdatePacket b), UpdatePacket b)] -- UpdateProcesses that send
  -> TMVar [(P.SendPort (UpdatePacket b), UpdatePacket b)] -- Updates to send
  -> Time.TimeInterval
  -> P.Process ()
sendStateProcessSTM updaters v r = do
  -- timeRef <- P.liftIO $ createTimeRef
  forever $ delay >> do
    msgs <- readV v
    -- now  <- P.liftIO $ getCurrentTime
    -- sendUpdate (csv1, csv2) msgs
    sendUpdates updaters msgs
    -- then' <- P.liftIO $ getCurrentTime
    -- P.liftIO $ print $ "send via sender took: " ++ show (diffUTCTime then' now)
    -- dt <- P.liftIO $ senseTime timeRef
    -- P.liftIO . print $ "Process send rate:" ++ show dt
    return ()
 where
  readV v' = P.liftIO . atomically $ takeTMVar v'
  delay = P.liftIO $ threadDelay (Time.asTimeout r)

-- Send Updates to list of UpdateProcesses by writing to their TMVars.
-- UpdateProcesses are cycled through, so if the number of messages exceeds the number of UpdateProcesses, some will send multiple messages.
sendUpdates
  :: (Binary b, Typeable b)
  => [TMVar (P.SendPort b, b)]
  -> [(P.SendPort b, b)]
  -> P.Process ()
sendUpdates updaters ms = forM_ zipped (uncurry writeV)
 where
  zipped = zip (cycle updaters) ms
  writeV v m = P.liftIO . atomically $ putTMVar v m

-- Spawn a number of UpdateProcesses
spawnUpdateProcesses
  :: (Binary b, Typeable b)
  => Integer
  -> P.Process [(TMVar (P.SendPort b, b), P.ProcessId)]
spawnUpdateProcesses n = forM [1 .. n] (\_ -> spawnUpdateProcess)

-- Spawn a process that sends messages on a typed channel. Messages to send are transmitted via the returned TMVar. Process can be quit via the returned ProcessId.
spawnUpdateProcess
  :: (Binary b, Typeable b)
  => P.Process (TMVar (P.SendPort (b), b), P.ProcessId)
spawnUpdateProcess = do
  v   <- P.liftIO $ newEmptyTMVarIO
  pid <- P.spawnLocal $ updateProcess v
  return (v, pid)

-- Never ending process to send updates to SendPorts as soon as they are available in the TMVar.
updateProcess
  :: (Binary b, Typeable b) => TMVar (P.SendPort b, b) -> P.Process ()
updateProcess v = forever $ do
  msg <- readVar v
  uncurry P.sendChan msg
  where readVar = P.liftIO . atomically . takeTMVar


-- Sensing time --

-- TODO move next two functions into separate module
-- createTimeRef :: IO (IORef UTCTime)
-- createTimeRef = getCurrentTime >>= newIORef

-- senseTime :: IORef UTCTime -> IO NominalDiffTime
-- senseTime timeRef = do
--   newTime      <- getCurrentTime
--   previousTime <- readIORef timeRef
--   writeIORef timeRef $ newTime
--   return $ diffUTCTime newTime previousTime

-- Other functions --

-- Blocks until the state satisfies a certain condition
waitUntilState
  :: (HasState s a) => s -> (ServerState a -> Bool) -> IO (ServerState a)
waitUntilState hs f = atomically $ do
  s <- getStateSTM hs
  when (not $ f s) retry
  return s


-- Api Handlers --

-- runs resultP to decide whether req is accepted. Accepted clients will be added to the end of the current state. Result will be sent to the client.
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
handleCommandPacket q s m@(CommandPacket pid _ _) = do
  when (pid `elem` (ids <$> s)) (writeQ m)
  MP.continue s
 where
  writeQ = P.liftIO . atomically . writeTQueue q
  ids =
    P.sendPortProcessId . P.sendPortId . serverStateSendPort . serverStateClient

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

