-- | This module exports all functions necessarry to create a client application.

{-# LANGUAGE FlexibleContexts #-}

module Network.Client
  ( startClientProcess
  , searchForServer
  , createServerStateChannel
  , LocalClient(..)
  , writeCommand
  , receiveState
  , Server(..)
  )
where

import           Network.Common
import           Network.Server
import           Control.Concurrent
import           Control.Concurrent.STM.TMVar
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Node
                                               as Node
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.STM
import           Data.ByteString.Char8
import qualified Network.Transport             as T

-- | A handler to a locally running client process
data LocalClient a b = LocalClient {
                       -- | 'ProcessId' of the main client process
                       pidClient :: P.ProcessId
                       -- | 'TMVar' containing incoming state. Content is replaced on successive UpdatePackets
                     , readVar :: TMVar (UpdatePacket a)
                       -- | Write in this 'TMVar' to immediately send a CommandPacket
                     , sendVar :: TMVar (CommandPacket b)
                     }

-- | A server a client can connect to
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

-- | Starts a client, and tries to join a server
startClientProcess
  :: (Binary a, Typeable a, Binary b, Typeable b)
  => Node.LocalNode
  -> Server
  -> String
  -> P.Process (ServerStateChannel a)
  -> IO (LocalClient a b, TMVar (JoinRequestResult [Nickname]))
startClientProcess node server nick sChanP = do
  rVar          <- newEmptyTMVarIO :: IO (TMVar (UpdatePacket a))
  sVar          <- newEmptyTMVarIO :: IO (TMVar (CommandPacket b))
  joinResultVar <- newEmptyTMVarIO
  pid           <- Node.forkProcess node $ catch
    (do

      ServerStateChannel sp rp <- sChanP
      joinResult               <- sendJoinRequest server nick sp

      P.liftIO . atomically $ putTMVar joinResultVar joinResult

      case joinResult of
        JoinRequestResult r -> case r of
          Left  err -> P.liftIO $ print err
          Right acc -> do
            P.liftIO $ print $ "join successful: " ++ show acc
            clientProcess node server rp rVar sVar
    )
    (\e -> do
        P.liftIO $ print "client exception"
        P.liftIO $ print $ show (e :: SomeException)
        throwM e
    )
  return (LocalClient pid rVar sVar, joinResultVar)

clientProcess
  :: (Binary a, Typeable a, Binary b, Typeable b)
  => Node.LocalNode
  -> Server
  -> ServerStateReceivePort a
  -> TMVar (UpdatePacket a)
  -> TMVar (CommandPacket b)
  -> P.Process ()
clientProcess node server rp rVar sVar = do
  P.liftIO $ print $ "LocalClient starts at: " ++ show
    (P.nodeAddress $ Node.localNodeId node)

  case server of
    Server pid -> P.link pid

  inPid  <- P.liftIO $ Node.forkProcess node (receiveStateProcess rVar rp)
  outPid <- P.liftIO $ Node.forkProcess node (sendCommandProcess sVar server)

  P.link inPid
  P.link outPid

  _ <- P.liftIO $ forever $ threadDelay 100000
  P.liftIO $ print "clientProcess ends"

-- | Creates a typed channel used by servers to send UpdatePackets to clients.
createServerStateChannel
  :: (Binary a, Typeable a) => P.Process (ServerStateChannel a)
createServerStateChannel =
  (\(s, r) ->
      ServerStateChannel (ServerStateSendPort s) (ServerStateReceivePort r)
    )
    <$> P.newChan

-- | Search for a server process. Needs to be registered as 'name' on a Node identified by 'server' which will be mapped to a 'NodeId'
searchForServer :: String -> String -> P.Process (Maybe Server)
searchForServer name server = do
  P.liftIO
    $  print
    $  "searching server process "
    ++ name
    ++ " - "
    ++ show serverNode
  (fmap . fmap) Server (searchProcessTimeout name serverNode 5000)
 where
  serverEndpoint = T.EndPointAddress $ pack server
  serverNode     = P.NodeId serverEndpoint


-- | Process to receive UpdatePackets sent from the server
receiveStateProcess
  :: (Binary a, Typeable a)
  => TMVar (UpdatePacket a)
  -> ServerStateReceivePort a
  -> P.Process ()
receiveStateProcess q p =
  forever $ P.receiveChan (serverStateReceivePort p) >>= writeQ
  where writeQ = P.liftIO . atomically . replaceTMVar q

-- | Process to send CommandPackets to the server
sendCommandProcess
  :: (Binary a, Typeable a) => TMVar (CommandPacket a) -> Server -> P.Process ()
sendCommandProcess q s = forever $ readQ q >>= sendState
 where
  readQ q' = P.liftIO . atomically $ takeTMVar q'
  sendState = clientUpdate s

-- | Send a 'JoinRequest' that contains the client's 'NickName' and the 'SendPort' to receive state updates
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

-- | Put a Command into a 'TMVar' 'q'. If 'q' is currently full, retries until TMVar is empty.
writeCommand :: TMVar (CommandPacket cmd) -> CommandPacket cmd -> IO ()
writeCommand q c = atomically $ putTMVar q c

-- | Returns the last received 'UpdatePacket'. If 'TMVar' is empty, returns 'Nothing'.
receiveState :: TMVar (UpdatePacket a) -> IO (Maybe (UpdatePacket a))
receiveState = atomically . tryTakeTMVar

