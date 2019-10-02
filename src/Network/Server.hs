module Network.Server where

import Network.Common

import Control.Concurrent
import qualified Control.Distributed.Process as P
import qualified Control.Distributed.Process.Extras.SystemLog as Log
import qualified Control.Distributed.Process.Extras.Time as Time
import Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process.ManagedProcess as MP
import qualified Control.Distributed.Process.Node as Node
import Control.Monad
import qualified Network.Socket as N
import qualified Network.Transport.TCP as NT
import qualified Network.Transport as T

type Client = (String, T.EndPoint)
type ServerState = [Client]

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
                        , MP.handleCall_ callJoinRequest
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
                    , MP.unhandledMessagePolicy = MP.Terminate
                    }

callJoinRequest :: JoinRequest -> P.Process JoinRequestResult
callJoinRequest msg = do
  P.liftIO $ print $ "JoinRequest: " ++ (show msg)
  return $ JoinRequestResult $ Right JoinAccepted

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
logShutdown _ reason =
  P.liftIO $ print $ "logShutdown: " ++ (show reason)

