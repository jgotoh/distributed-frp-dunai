module Network.Client where

import           Network.Common
import           Control.Concurrent.STM.TQueue
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import qualified Control.Distributed.Process.Extras.Timer
                                               as Timer
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import qualified Control.Distributed.Process.Node
                                               as Node
import           Control.Exception.Base
import           Data.ByteString.Char8
import qualified Network.Socket                as N
import qualified Network.Transport.TCP         as NT
import qualified Network.Transport             as T

data Client a = Client { clientPid :: P.ProcessId
                     , sendQueue :: TQueue (StateUpdate a)
                     , readQueue :: TQueue (StateUpdate a)
                     }

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

startClientNetworkProcess :: Node.LocalNode -> IO (Client a)
startClientNetworkProcess node = do

  sendQueue <- newTQueueIO
  readQueue <- newTQueueIO
  pid       <- Node.forkProcess node $ do
    -- start Netin process
    -- start Netout process
    undefined
  --return $ Client $ pid sendQueue readQueue
  undefined

getLocalAddress :: Node.LocalNode -> T.EndPointAddress
getLocalAddress node = P.nodeAddress $ Node.localNodeId node

searchForServer :: String -> String -> P.Process (Maybe P.ProcessId)
searchForServer name server = do
  P.liftIO
    $  print
    $  "searching server process "
    ++ name
    ++ " - "
    ++ show serverNode
  searchProcessTimeout name serverNode 1000
 where
  serverEndpoint = T.EndPointAddress $ pack server
  serverNode     = P.NodeId serverEndpoint


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
            joinResult <- sendJoinRequest serverPid nick sp
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
  :: P.ProcessId
  -> Nickname
  -> P.SendPort (StateUpdate Message)
  -> P.Process (JoinRequestResult [Nickname])
sendJoinRequest pid nick port = do
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

