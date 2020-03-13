{-# LANGUAGE FlexibleContexts #-}

module Network.Client
  ( startClientProcess
  , searchForServer
  , createServerStateChannel
  , LocalClient(..)
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

data LocalClient a b = LocalClient { clientPid :: P.ProcessId
                     , readVar :: TMVar (UpdatePacket a)
                     , sendVar :: TMVar (CommandPacket b)
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
  :: (Binary a, Typeable a, Binary b, Typeable b)
  => Node.LocalNode
  -> Server
  -> String
  -> P.Process (ServerStateChannel a)
  -> IO (LocalClient a b, TMVar (JoinRequestResult [Nickname]))
startClientProcess node server nick sChanP = do
  rVar <- newEmptyTMVarIO :: IO (TMVar (UpdatePacket a))
  sVar <- newEmptyTMVarIO :: IO (TMVar (CommandPacket b))
  joinResultVar   <- newEmptyTMVarIO
  pid    <- Node.forkProcess node $ catch
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
    (\e -> P.liftIO $ print $ show (e :: SomeException))
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
  outPid <- P.liftIO $ Node.forkProcess
    node
    (sendStateProcess sVar server)

  P.link inPid
  P.link outPid

  _ <- P.liftIO $ forever $ threadDelay 100000
  P.liftIO $ print "clientProcess ends"

-- Creates a typed channel used by servers to send UpdatePackets to clients.
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
  (fmap . fmap) Server (searchProcessTimeout name serverNode 5000)
 where
  serverEndpoint = T.EndPointAddress $ pack server
  serverNode     = P.NodeId serverEndpoint


-- Process to receive UpdatePackets sent from the server
receiveStateProcess
  :: (Binary a, Typeable a)
  => TMVar (UpdatePacket a)
  -> ServerStateReceivePort a
  -> P.Process ()
receiveStateProcess q p =
  forever $ P.receiveChan (serverStateReceivePort p) >>= writeQ
  where
    writeQ = P.liftIO . atomically . replaceTMVar q

-- Empties a TMVar, then writes a new value
replaceTMVar :: TMVar a -> a -> STM ()
replaceTMVar v a = do
  empty' <- isEmptyTMVar v
  if empty'
    then putTMVar v a
    else do
      _ <- swapTMVar v a
      return ()

-- Process to send CommandPackets to the server
sendStateProcess
  :: (Binary a, Typeable a)
  => TMVar (CommandPacket a)
  -> Server
  -> P.Process ()
sendStateProcess q s = forever $ readQ q >>= sendState
 where
  readQ q' = P.liftIO . atomically $ takeTMVar q'
  sendState = clientUpdate s

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

