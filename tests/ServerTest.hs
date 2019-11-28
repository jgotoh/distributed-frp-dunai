{-# LANGUAGE DeriveGeneric #-}

module ServerTest
  ( serverTests
  )
where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception       hiding ( catch )
import           Control.Monad.Catch
import           Control.Monad
import           FRP.BearRiver
import           Network.Common          hiding ( Client(..)
                                                , Message(..)
                                                )
import           Network.AuthoritativeServer
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Node
                                               as Node
import qualified Control.Distributed.Process.Extras.SystemLog
                                               as Log
-- import qualified Control.Distributed.Process.ManagedProcess
                                               -- as MP
import           Control.Distributed.Process.Extras
-- import qualified Network.Transport.TCP         as NT

import qualified Network.Transport             as T
data TestMessage = Ping | Pong
  deriving (Generic, Show, Typeable, Eq)
instance Binary TestMessage

type TestState = StateUpdate TestMessage

testIp :: HostName
testIp = "localhost"

-- requesting port 0 tells the system to automatically find an unused port
testPort :: ServiceName
testPort = "0"

testSession :: SessionName
testSession = "testSession"

testConfiguration :: Node.LocalNode -> ServerConfiguration TestMessage
testConfiguration n =
  defaultServerConfig n testIp testPort testSession defaultFRPProcessDefinition

serverTests :: TestTree
serverTests = withResource withNode clearNode tests
 where
  withNode = initializeNode testIp testPort >>= \n -> case n of
    Left  err        -> error $ show err
    Right nt@(n', _) -> do
      Node.runProcess n' $ do
        _ <- Log.systemLog (P.liftIO . print) (return ()) Log.Debug return
        return ()
      return nt
  clearNode (n, t) = do
    print "closing node now"
    Node.closeLocalNode n >> T.closeTransport t

tests :: IO (Node.LocalNode, T.Transport) -> TestTree
tests mkNT = testGroup
  "ServerTests"
  [ testCase "testing default configuration" $ mkNT >>= testDefaultServer
  , testCase "testing join requests" $ mkNT >>= testJoinRequests
  , testCase "testing clientUpdate" $ mkNT >>= testClientUpdates
  ]

-- TODO Tests a server that is running a simulation
-- Snapshots of the simulation (StateUpdate) are sent at a static rate
-- s -> c updaterate
-- test sendStateProcess

-- Tests whether updates sent by clientUpdate are correctly added to the receivingQueue of a server
testClientUpdates :: (Node.LocalNode, T.Transport) -> Assertion
testClientUpdates (n, _) = withServer
  (startServerProcess (testConfiguration n))
  test
  n
 where
  nick1 = "1"
  test server = Node.runProcess n $ do

    (Right sPid)   <- P.liftIO . atomically . readTMVar $ pidApiServer server

    (sp1v, sp1pid) <- P.liftIO $ testProcess n
    sp1            <- P.liftIO . atomically $ readTMVar sp1v

    let ssp1   = ServerStateSendPort sp1
        join1  = JoinRequest nick1 ssp1
        update = StateUpdate sp1pid Ping

    -- Send StateUpdate without first connecting to the server
    -- the Server should ignore it
    clientUpdate sPid update
    clientUpdate sPid update
    P.liftIO $ threadDelay 1000000
    q <- P.liftIO . atomically . flushTQueue $ readQueue server
    P.liftIO $ [] @=? q

    -- Connect and send again
    _ <- joinRequest sPid join1
    clientUpdate sPid update
    P.liftIO $ threadDelay 1000000
    q <- P.liftIO . atomically . flushTQueue $ readQueue server
    P.liftIO $ [update] @=? q

    clientUpdate sPid update
    clientUpdate sPid update
    P.liftIO $ threadDelay 1000000
    q <- P.liftIO . atomically . flushTQueue $ readQueue server
    P.liftIO $ [update, update] @=? q

testDefaultServer :: (Node.LocalNode, T.Transport) -> Assertion
testDefaultServer (n, _) = withServer
  (startServerProcess (testConfiguration n))
  test
  n
 where
  test server = do
    Node.runProcess n $ do

      let serverPid = pidServer server
          vPid      = pidApiServer server

      (Right apiPid) <- P.liftIO $ atomically $ readTMVar vPid
      main           <- isProcessAlive serverPid
      api            <- isProcessAlive apiPid

      P.liftIO $ main @? "main process is not active"
      P.liftIO $ api @? "api process is not active"

-- Tests handling of JoinRequests and monitoring of ServerStateSendPorts
testJoinRequests :: (Node.LocalNode, T.Transport) -> Assertion
testJoinRequests (n, _) = withServer (startServerProcess cfg) (test n) n
 where
  cfg :: ServerConfiguration TestMessage
  cfg = (testConfiguration n) { joinConfig = twoClients }
  twoClients xs _ = return $ JoinRequestResult $ if length xs < 2
    then (Right $ JoinAccepted $ nicks xs)
    else (Left $ JoinError errorMsg)
  nicks xs = map nameClient xs
  errorMsg = "error message"
  nick1    = "1"
  nick2    = "2"
  nick3    = "3"
  test node server = Node.runProcess node $ do

    (Right sPid)   <- P.liftIO . atomically . readTMVar $ pidApiServer server

    -- Create 3 Processes with SendPorts to connect to server
    (sp1v, sp1pid) <- P.liftIO $ testProcess n
    (sp2v, sp2pid) <- P.liftIO $ testProcess n
    (sp3v, sp3pid) <- P.liftIO $ testProcess n

    sp1            <- P.liftIO $ atomically $ readTMVar sp1v
    sp2            <- P.liftIO $ atomically $ readTMVar sp2v
    sp3            <- P.liftIO $ atomically $ readTMVar sp3v

    let ssp1  = ServerStateSendPort sp1
        ssp2  = ServerStateSendPort sp2
        ssp3  = ServerStateSendPort sp3
        join1 = JoinRequest nick1 ssp1
        join2 = JoinRequest nick2 ssp2
        join3 = JoinRequest nick3 ssp3

    (JoinRequestResult (Right (JoinAccepted xs1))) <- joinRequest sPid join1
    (JoinRequestResult (Right (JoinAccepted xs2))) <- joinRequest sPid join2
    (JoinRequestResult (Left  (JoinError    err))) <- joinRequest sPid join3

    P.liftIO $ xs1 @?= []
    P.liftIO $ xs2 @?= [nick1]
    P.liftIO $ err @?= errorMsg

    -- test getState
    state <- P.liftIO $ getState server

    let expectedClient1 = Client nick1 ssp1
        expectedClient2 = Client nick2 ssp2
        expectedState   = [expectedClient2, expectedClient1]

    P.liftIO $ expectedState @=? state

    -- then remove clients, and test getState again
    P.kill sp1pid "removing client 1"
    -- the notification the server receives when a client quits is delivered asynchronously, so we first wait an arbitrary amount of time
    P.liftIO $ threadDelay 1000000
    state <- P.liftIO $ getState server
    let expectedState = [expectedClient2]
    P.liftIO $ expectedState @=? state

    P.kill sp2pid "removing client 2"
    P.liftIO $ threadDelay 1000000
    state <- P.liftIO $ getState server
    P.liftIO $ [] @=? state

-- Starts a process that creates a channel for state exchanges and checks forever for incoming messages
testProcess :: Node.LocalNode -> IO (TMVar (P.SendPort TestState), P.ProcessId)
testProcess n = do
  v   <- newEmptyTMVarIO
  pid <- Node.forkProcess n $ do
    (sp, rp) <-
      P.newChan :: P.Process (P.SendPort TestState, P.ReceivePort TestState)
    P.liftIO . atomically $ putTMVar v sp
    P.linkPort sp
    forever $ P.receiveChan rp
  return (v, pid)

-- Create a server in a computation, pass it to the test and then shutdown the server
withServer
  :: IO (LocalServer a)
  -> (LocalServer a -> Assertion)
  -> Node.LocalNode
  -> Assertion
withServer mkServer test n = do
  -- TODO use bracket
  server    <- mkServer
  (Right _) <- atomically $ readTMVar (pidApiServer server)
  test server
  exitServer server
 where
  exitServer s = do
    Node.runProcess n
      $  exitProc s "exiting server via withServer"
      >> P.unregister testSession

-- TODO test:
-- servers that run simulations themselves
-- test sending of stateUpdates (s -> c)
-- joining leaving simulations in progress
-- # test clientUpdates are written to rQueue
-- return initial state of world on join
-- # library users can decide whether clients can join
-- # servers need to be handle a varying amount of connected clients
-- clients do not need to determine the destination of messages (responsibility of servers)
-- users of the library need to be able to add new message types
-- # state Updates are generic, users of the library can decide what types of data should be transmitted and how network data is processed
-- servers need way to run simulations themselves

