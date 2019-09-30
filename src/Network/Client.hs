module Network.Client where

import Network.Common
import qualified Control.Distributed.Process as P
import qualified Control.Distributed.Process.Extras.Time as Time
import qualified Control.Distributed.Process.Extras.Timer as Timer
import qualified Control.Distributed.Process.ManagedProcess as MP
import qualified Control.Distributed.Process.Node as Node
import Data.ByteString.Char8
import qualified Network.Socket as N
import qualified Network.Transport.TCP as NT
import qualified Network.Transport as T

launchClient :: N.HostName -> N.ServiceName -> String -> String -> IO ()
launchClient ip port server name = do
  transport <- NT.createTransport (NT.defaultTCPAddr ip port) NT.defaultTCPParameters
  case transport of
    Left failure -> print $ show failure
    Right success -> do
      print "successfully connected to client socket"

      node <- createLocalNode success
      Node.runProcess node $ do

        let clientAddress = P.nodeAddress $ Node.localNodeId node :: T.EndPointAddress
        P.liftIO $ print $ "Client starts at: " ++ (show $ clientAddress)

        let serverEndpoint = T.EndPointAddress $ pack server
            serverNode = P.NodeId serverEndpoint

        P.liftIO $ print $ "searching server process " ++ name ++ " - " ++ (show serverNode)
        maybePid <- searchProcessTimeout name serverNode 1000

        case maybePid of
          Just serverPid -> do
            P.link serverPid
            callPingLoop serverPid
            return ()
          Nothing -> return ()
  return ()

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
  P.liftIO $ print $ "received: " ++ (show pong)
  Timer.sleepFor 500 Time.Millis
  callPingLoop pid

