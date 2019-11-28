{-# LANGUAGE FlexibleContexts #-}

module Network.Client
  ( startClientProcess
  , searchForServer
  , createServerStateChannel
  , runProcessResult
  , LocalClient(..)
  )
where

import           Network.Common
import           Network.Server
import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TMVar
import qualified Control.Distributed.Process   as P
import           Control.Distributed.Process.Extras
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import qualified Control.Distributed.Process.Node
                                               as Node
-- import           Control.Exception.Base         ( IOException )
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.STM
import           Data.ByteString.Char8
import qualified Network.Transport             as T

data LocalClient a = LocalClient { clientPid :: P.ProcessId
                     , sendQueue :: TQueue (StateUpdate a)
                     , readQueue :: TQueue (StateUpdate a)
                     }

newtype Server = Server P.ProcessId
  deriving Show

instance Addressable Server

instance Resolvable Server where
  resolve a = case a of
    Server pid -> return $ Just pid
  unresolvableMessage a = "Server could not be resolved: " ++ show a

instance Routable Server where
  sendTo s m = resolve s >>= maybe (error $ unresolvableMessage s) (`P.send` m)
  unsafeSendTo s m =
    resolve s >>= maybe (error $ unresolvableMessage s) (`P.unsafeSend` m)

-- Starts a client, searches for a server and tries to join it
startClientProcess
  :: (Binary a, Typeable a)
  => Node.LocalNode
  -> Server
  -> String
  -> P.Process (ServerStateChannel a)
  -> IO (LocalClient a, TMVar (JoinRequestResult [Nickname]))
startClientProcess node server nick sChanP = do
  sQueue <- newTQueueIO
  rQueue <- newTQueueIO
  rVar   <- newEmptyTMVarIO
  pid    <- Node.forkProcess node $ catch
    (do

      ServerStateChannel sp rp <- sChanP
      joinResult               <- sendJoinRequest server nick sp

      P.liftIO . atomically $ putTMVar rVar joinResult

      case joinResult of
        JoinRequestResult r -> case r of
          Left  err -> P.liftIO $ print err
          Right acc -> do
            P.liftIO $ print $ "join successful: " ++ show acc
            clientProcess node server rp rQueue sQueue
    )
    (\e -> P.liftIO $ print $ show (e :: SomeException))
  return (LocalClient pid sQueue rQueue, rVar) -- return $ (LocalClient _, CurrentState)

-- TODO create something along the lines of P.ProcessDefinition for clients
-- TODO cmdrate as argument
clientProcess
  :: (Binary a, Typeable a)
  => Node.LocalNode
  -> Server
  -> ServerStateReceivePort a
  -> TQueue (StateUpdate a)
  -> TQueue (StateUpdate a)
  -> P.Process ()
clientProcess node server rp rQueue sQueue = do
  P.liftIO $ print $ "LocalClient starts at: " ++ show
    (P.nodeAddress $ Node.localNodeId node)

  case server of
    Server pid -> P.link pid

  inPid  <- P.liftIO $ Node.forkProcess node (receiveStateProcess rQueue rp)
  outPid <- P.liftIO $ Node.forkProcess
    node
    (sendStateProcess sQueue server (Time.milliSeconds 16))

  P.link inPid
  P.link outPid

  P.liftIO $ print "clientProcess now waits"
  _ <- P.liftIO $ forever $ threadDelay 100000
  P.liftIO $ print "clientProcess ends"

-- Creates a typed channel used by servers to send StateUpdates to clients.
createServerStateChannel
  :: (Binary a, Typeable a) => P.Process (ServerStateChannel a)
createServerStateChannel =
  (\(s, r) ->
      ServerStateChannel (ServerStateSendPort s) (ServerStateReceivePort r)
    )
    <$> P.newChan

searchForServer :: String -> String -> P.Process (Maybe Server)
searchForServer name server = do
  P.liftIO
    $  print
    $  "searching server process "
    ++ name
    ++ " - "
    ++ show serverNode
  (fmap . fmap) Server (searchProcessTimeout name serverNode 1000)
 where
  serverEndpoint = T.EndPointAddress $ pack server
  serverNode     = P.NodeId serverEndpoint

-- runs a Process, blocks until it finishes. Result contains a value if the process did not terminate unexpected
-- TODO catch exceptions
runProcessResult :: Node.LocalNode -> P.Process a -> IO (Maybe a)
runProcessResult node p = do
  v <- newEmptyTMVarIO
  Node.runProcess node $ do
    result <- p
    P.liftIO $ atomically $ putTMVar v result
  atomically $ tryReadTMVar v

-- Process to receive StateUpdates sent from the server
receiveStateProcess
  :: (Binary a, Typeable a)
  => TQueue (StateUpdate a)
  -> ServerStateReceivePort a
  -> P.Process ()
receiveStateProcess q p =
  forever $ P.receiveChan (serverStateReceivePort p) >>= writeQ
  where writeQ = P.liftIO . atomically . writeTQueue q

-- Process to send StateUpdates to the server
sendStateProcess
  :: (Binary a, Typeable a)
  => TQueue (StateUpdate a)
  -> Server
  -> CommandRate
  -> P.Process ()
sendStateProcess q s r = forever $ delay >> readQ q >>= sendState
 where
  readQ q' = P.liftIO . atomically $ flushTQueue q'
  -- TODO sendState currently only sends the newest state
  sendState (x : _) = clientUpdate s x
  sendState []      = return ()
  delay = P.liftIO $ threadDelay (Time.asTimeout r)

-- send a JoinRequest that contains the client's nickname and the SendPort to receive simulation state updates
sendJoinRequest
  :: (Binary a, Typeable a)
  => Server
  -> Nickname
  -> ServerStateSendPort a
  -> P.Process (JoinRequestResult [Nickname])
sendJoinRequest s nick (ServerStateSendPort sp) = do
  P.liftIO $ print $ "send joinRequest: " ++ show request
  joinRequest s request
  where request = JoinRequest nick (ServerStateSendPort sp)

