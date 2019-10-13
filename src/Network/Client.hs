{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE PolyKinds, ConstraintKinds, TypeFamilies, UndecidableInstances, DataKinds, TypeOperators #-}
module Network.Client where

import           Network.Common
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TMVar
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import qualified Control.Distributed.Process.Extras.Timer
                                               as Timer
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import qualified Control.Distributed.Process.Node
                                               as Node
import Control.Distributed.Process.Serializable
import           Control.Exception.Base
import           Control.Monad.STM
import Data.Binary
import           Data.ByteString.Char8
import           GHC.Generics                   ( Generic )
import qualified Network.Socket                as N
import qualified Network.Transport.TCP         as NT
import qualified Network.Transport             as T
import Type.Reflection

data Client a = Client { clientPid :: P.ProcessId
                     , sendQueue :: TQueue (StateUpdate a)
                     , readQueue :: TQueue (StateUpdate a)
                     }
--data Client a where
--  Client :: (Serializable a) => P.ProcessId -> TQueue (StateUpdate a) -> TQueue (StateUpdate a) -> Client a

newtype Server = Server P.ProcessId

-- TODO this is actually not possible because of newChan :: Serializable a => Process (SendPort a, ReceivePort a). Does it actually make sense?
newtype ControlChannel a = ControlChannel (P.SendPort JoinRequest, P.ReceivePort (JoinRequestResult a))

-- Channel used by the server to send StateUpdates to clients
data ServerStateChannel a where
  ServerStateChannel :: (Serializable a) => (ServerStateSendPort a) -> (ServerStateReceivePort a) -> ServerStateChannel a

-- Channel used by clients to send StateUpdates to a server
data ClientStateChannel a where
  ClientStateChannel :: (Serializable a) => (ClientStateSendPort a) -> (ClientStateReceivePort a) -> ClientStateChannel a

-- Port used by a server to send StateUpdates to a client. Server -[state]-> Client
newtype ServerStateSendPort a = ServerStateSendPort (P.SendPort (StateUpdate a))
  deriving (Generic)

-- Port used by a client to receive StateUpdates from a server. Server -[state]-> Client
newtype ServerStateReceivePort a = ServerStateReceivePort (P.ReceivePort (StateUpdate a))
  deriving (Generic)

-- Port used by a client to send StateUpdates to a server. Client -[state]-> Server
newtype ClientStateSendPort a = ClientStateSendPort (P.SendPort (StateUpdate a))
  deriving (Generic)

-- Port used by a server to receive StateUpdates from a client. Client -[state]-> Server
newtype ClientStateReceivePort a = ClientStateReceivePort (P.ReceivePort (StateUpdate a))
  deriving (Generic)

initializeClientNode
  :: N.HostName -> N.ServiceName -> IO (Either IOException Node.LocalNode)
initializeClientNode ip port = do
  -- TODO is Bifunctor.second usable here?
  t <- NT.createTransport (NT.defaultTCPAddr ip port) NT.defaultTCPParameters
  case t of
    Left  l -> return $ Left l
    Right r -> do
      n <- createLocalNode r
      return $ Right n

startClientNetworkProcess :: Serializable a => Node.LocalNode -> Server -> String -> IO (Client a)
startClientNetworkProcess node server nick = do
  sQueue <- newTQueueIO
  rQueue <- newTQueueIO
  pid    <- Node.forkProcess node $ do

    P.liftIO $ print $ "Client starts at: " ++ show
      (P.nodeAddress $ Node.localNodeId node)

    case server of
      Server pid -> do
        P.link pid

    let sp = undefined
    joinResult <- sendJoinRequest server nick sp

    case joinResult of
      JoinRequestResult e -> case e of
        Left  err -> P.liftIO $ print err
        Right acc -> do
          P.liftIO $ print $ "join successful: " ++ show acc

          clientStateChan <- createClientStateChannel' --  :: P.Process (ClientStateChannel String)
          --serverStateChan <- createServerStateChannel

          inPid <- P.liftIO $ Node.forkProcess node (receiveStateProcess rQueue)
          outPid <- P.liftIO $ Node.forkProcess node (sendStateProcess sQueue)

          P.link inPid
          P.link outPid
          P.liftIO $ print "client network process ends"
          -- send stateChan.receivePort in JoinRequest
  return $ Client pid sQueue rQueue

createClientStateChannel' :: Serializable a => P.Process a
createClientStateChannel' = undefined

createControlChannel :: P.Process (ControlChannel a)
createControlChannel = undefined --ControlChannel <$> P.newChan

createServerStateChannel :: Serializable a => P.Process (ServerStateChannel a)
createServerStateChannel = (\(s,r) -> ServerStateChannel (ServerStateSendPort s)  (ServerStateReceivePort r)) <$> P.newChan

createClientStateChannel :: (Binary a, Typeable a) =>  P.Process (ClientStateChannel a)
createClientStateChannel = (\(s,r) -> ClientStateChannel (ClientStateSendPort s)  (ClientStateReceivePort r)) <$> P.newChan

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
runProcessResult :: Node.LocalNode -> P.Process a -> IO (Maybe a)
runProcessResult node p = do
  v <- newEmptyTMVarIO
  Node.runProcess node $ do
    result <- p
    P.liftIO $ atomically $ putTMVar v result
  atomically $ tryReadTMVar v

-- Process to receive StateUpdates sent from the server
receiveStateProcess = undefined

-- Process to send StateUpdates to the server
sendStateProcess = undefined

launchClient
  :: N.HostName -> N.ServiceName -> String -> String -> String -> IO ()
launchClient ip port nick server name = do
  transport <- NT.createTransport (NT.defaultTCPAddr ip port)
                                  NT.defaultTCPParameters
  case transport of
    Left  failure -> print $ show failure
    Right success -> do
      print "successfully connected to client socket"

      node <- createLocalNode success
      Node.runProcess node $ do

        let clientAddress =
              P.nodeAddress $ Node.localNodeId node :: T.EndPointAddress
        P.liftIO $ print $ "Client starts at: " ++ show clientAddress

        let serverEndpoint = T.EndPointAddress $ pack server
            serverNode     = P.NodeId serverEndpoint

        P.liftIO
          $  print
          $  "searching server process "
          ++ name
          ++ " - "
          ++ show serverNode
        maybePid <- searchProcessTimeout name serverNode 1000

        case maybePid of
          Just serverPid -> do
            P.link serverPid
            (sp, rp)   <- P.newChan
            joinResult <- sendJoinRequest (Server serverPid) nick sp
            case joinResult of
              JoinRequestResult e -> case e of
                Left  err -> P.liftIO $ print err
                Right acc -> do
                  P.liftIO $ print $ "join successful: " ++ show acc
                  chanPingLoop serverPid rp (msg acc)
            return ()
          Nothing -> return ()
  return ()
  where msg (JoinAccepted nicks) = if Prelude.null nicks then Ping else Pong

-- send a JoinRequest that contains the client's nickname and the SendPort to receive simulation state updates
sendJoinRequest
  :: Server
  -> Nickname
  -> P.SendPort (StateUpdate Message)
  -> P.Process (JoinRequestResult [Nickname])
sendJoinRequest (Server pid) nick port = do
  P.liftIO $ print $ "send joinRequest: " ++ show request
  MP.call pid request :: P.Process (JoinRequestResult [Nickname])
  where request = JoinRequest nick port

castPingLoop :: P.ProcessId -> P.Process ()
castPingLoop pid = do
  P.liftIO $ print "cast Ping"
  MP.cast pid Ping
  Timer.sleepFor 500 Time.Millis
  castPingLoop pid

callPingLoop :: P.ProcessId -> P.Process ()
callPingLoop pid = do
  P.liftIO $ print "call Ping"
  pong <- MP.call pid Ping :: P.Process Message
  P.liftIO $ print $ "received: " ++ show pong
  Timer.sleepFor 500 Time.Millis
  callPingLoop pid

chanPingLoop
  :: P.ProcessId
  -> P.ReceivePort (StateUpdate Message)
  -> Message
  -> P.Process ()
chanPingLoop pid rp msg0 = do
  sendStateUpdate pid $ StateUpdate msg0
  msg <- P.receiveChan rp
  P.liftIO $ print $ "received: " ++ show msg
  Timer.sleepFor 500 Time.Millis
  chanPingLoop pid rp msg0

sendStateUpdate :: P.ProcessId -> StateUpdate Message -> P.Process ()
sendStateUpdate pid msg = do
  _ <- MP.callChan pid msg :: P.Process (P.ReceivePort ())
  return ()

