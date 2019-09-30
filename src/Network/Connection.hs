{-# LANGUAGE DeriveGeneric #-}

module Network.Connection where

import Control.Concurrent
import qualified Control.Distributed.Process as P
import qualified Control.Distributed.Process.Extras.SystemLog as Log
import qualified Control.Distributed.Process.Extras.Time as Time
import qualified Control.Distributed.Process.Extras.Timer as Timer
import Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process.ManagedProcess as MP
import qualified Control.Distributed.Process.Node as Node
import Control.Monad
import Data.Binary
import Data.ByteString.Char8
import Data.Either
import Debug.Trace
import qualified Network.Socket as N
import qualified Network.Transport.TCP as NT
import qualified Network.Transport as T
import Type.Reflection
import GHC.Generics (Generic)


data Message = Ping | Pong
  deriving (Generic, Show, Typeable)
instance Binary Message

data UnhandledMessage = Peng
  deriving (Generic, Show, Typeable)
instance Binary UnhandledMessage

launchServer :: N.HostName -> N.ServiceName -> String -> IO ()
launchServer ip port name = do
  Right transport <- NT.createTransport (NT.defaultTCPAddr ip port) NT.defaultTCPParameters
  node <- createLocalNode transport
  Node.runProcess node $ do

    _ <- Log.systemLog (P.liftIO . print) (return ()) Log.Debug return

    let serverAddress = P.nodeAddress $ Node.localNodeId node :: T.EndPointAddress
    pid <- P.spawnLocal pongServerProcess

    P.link pid

    P.liftIO $ print $ "Server starts at: " ++ (show $ serverAddress) ++ " ProcessId: " ++ (show pid)
    P.register name pid
    P.liftIO $ forever $ threadDelay 16
  return ()

pongServerProcess :: P.Process ()
pongServerProcess = MP.serve () (MP.statelessInit Time.NoDelay) serverProcessDef

serverProcessDef :: MP.ProcessDefinition ()
serverProcessDef = MP.statelessProcess {
                    MP.apiHandlers = [
                        MP.handleCall_ callPong
                        , MP.handleCast castPong
                                  ]
                    , MP.infoHandlers = [
                        MP.handleInfo logInfo
                                      ]
                    , MP.exitHandlers = [
                        MP.handleExit logExit
                                        ]
                    , MP.timeoutHandler = logTimeout
                    , MP.shutdownHandler = logShutdown
                    , MP.unhandledMessagePolicy = MP.Log
                    }

callPong :: Message -> P.Process Message
callPong x = do
  P.liftIO $ print $ "server: " ++ (show x)
  return Pong

castPong :: s -> Message -> MP.Action s
castPong s x = do
  P.liftIO $ print $ "server: " ++ (show x)
  MP.continue s

logInfo :: s -> Message -> MP.Action s
logInfo s msg = do
  P.liftIO $ print $ "logInfo: msg: " ++ (show msg)
  MP.continue s

logExit :: P.ProcessId -> s -> Message -> MP.Action s
logExit pid s msg = do
  P.liftIO $ print $ "logExit: " ++ (show pid) ++ " msg: " ++ (show msg)
  MP.continue s

logTimeout :: s -> Time.Delay -> MP.Action s
logTimeout s delay = do
  when (delay /= Time.NoDelay) $ P.liftIO $ print $ "logTimeout: " ++ (show delay)
  MP.continue s

logShutdown :: MP.ExitState s -> ExitReason -> P.Process ()
logShutdown s reason =
  P.liftIO $ print $ "logShutdown: " ++ (show reason)

-- Searches for a Process under address addr called name. When timeLeft runs out, returns Nothing
searchProcessTimeout :: String -> P.NodeId -> Int -> P.Process (Maybe P.ProcessId)
searchProcessTimeout name addr timeLeft
  | timeLeft <= 0 = do
      P.liftIO $ print $ "searchProcess ended due to timeout. No process " ++ name ++ " at address "
        ++ (show addr) ++ " could be found"
      return Nothing
  | otherwise = do
      P.whereisRemoteAsync addr name
      reply <- P.expectTimeout timeout
      case reply of
        Just (P.WhereIsReply name' maybeID) -> case maybeID of
          Just pid -> do
            P.liftIO $ print $ "WhereIsReply " ++ name' ++ " pid: " ++ (show pid)
            return $ Just pid
          Nothing -> searchProcessTimeout name addr (timeLeft - timeoutMS)
        Nothing -> searchProcessTimeout name addr (timeLeft - timeoutMS)
  where
    timeoutMS = 1000
    timeout = Time.asTimeout $ Time.milliSeconds timeoutMS

launchClient :: N.HostName -> N.ServiceName -> String -> String -> IO ()
launchClient ip port server name = do
  let serverAddr = T.EndPointAddress $ pack server
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

createLocalNode :: T.Transport -> IO Node.LocalNode
createLocalNode transport = do
  Node.newLocalNode transport Node.initRemoteTable

