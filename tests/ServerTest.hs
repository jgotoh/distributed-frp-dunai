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
import           Network.Common
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
  deriving (Generic, Show, Typeable)
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
  defaultServerConfig n testIp testPort testSession defaultFRPServerDefinition

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
  clearNode (n, t) = Node.closeLocalNode n >> T.closeTransport t

tests :: IO (Node.LocalNode, T.Transport) -> TestTree
tests mkNT = testGroup
  "ServerTests"
  [ testCase "testing default configuration" $ mkNT >>= testDefaultServer
  -- deactivated, because SendPorts sent to a server mysteriously die.
  -- works in distributed-paddles
  -- , testCase "testing join requests" $ mkNT >>= testJoinRequests
  , testCase "testing join requests" $ mkNT >>= testJoinRequests
  , testCase "test simulating server" $ mkNT >>= testSimulatingServer
  ]

testSimulatingServer :: (Node.LocalNode, T.Transport) -> Assertion
testSimulatingServer (_, _)= do
  return ()

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

testJoinRequests :: (Node.LocalNode, T.Transport) -> Assertion
testJoinRequests (n, _) = withServerClients (startServerWithClients cfg reqs)
                                            (test n)
                                            n
 where
  cfg :: ServerConfiguration TestMessage
  cfg = (testConfiguration n) { joinConfig = twoClients }
  twoClients xs _ = return $ JoinRequestResult $ if length xs < 2
    then (Right $ JoinAccepted $ nicks xs)
    else (Left $ JoinError joinErrorMsg)
  joinErrorMsg = "joinError"
  reqs         = do

    (sp1, _) <-
      P.newChan :: P.Process (P.SendPort TestState, P.ReceivePort TestState)
    (sp2, _) <-
      P.newChan :: P.Process (P.SendPort TestState, P.ReceivePort TestState)
    (sp3, _) <-
      P.newChan :: P.Process (P.SendPort TestState, P.ReceivePort TestState)

    let join1 = JoinRequest "1" (ServerStateSendPort sp1)
        join2 = JoinRequest "2" (ServerStateSendPort sp2)
        join3 = JoinRequest "3" (ServerStateSendPort sp3)
    return [join1, join2, join3]

  nicks xs = map nameClient xs
  test node _ joinResults = Node.runProcess node $ do

    let expected1 = JoinRequestResult $ Right $ JoinAccepted []
        expected2 = JoinRequestResult $ Right $ JoinAccepted ["1"]
        expected3 = JoinRequestResult $ Left $ JoinError joinErrorMsg
        expected  = [expected1, expected2, expected3]

    P.liftIO $ expected @=? joinResults

-- Fails because SendPorts sent via JoinRequests mysteriously die
testJoinRequests' :: (Node.LocalNode, T.Transport) -> Assertion
testJoinRequests' (n, _) = withServer (startServerProcess cfg) (test n) n
 where
  cfg :: ServerConfiguration TestMessage
  cfg = (testConfiguration n) { joinConfig = oneClient }
  oneClient xs _ = return $ JoinRequestResult $ if length xs <= 2
    then (Right $ JoinAccepted $ nicks xs)
    else (Left $ JoinError "err")
  nicks xs = map nameClient xs
  test node server = Node.runProcess node $ do

    let vPid = pidApiServer server
    (Right sPid) <- P.liftIO $ atomically $ readTMVar vPid

    (sp1, _)     <-
      P.newChan :: P.Process (P.SendPort TestState, P.ReceivePort TestState)
    (sp2, _) <-
      P.newChan :: P.Process (P.SendPort TestState, P.ReceivePort TestState)
    (sp3, _) <-
      P.newChan :: P.Process (P.SendPort TestState, P.ReceivePort TestState)

    P.linkPort sp1
    P.linkPort sp2
    P.linkPort sp3

    let join1 = JoinRequest "1" (ServerStateSendPort sp1)
        join2 = JoinRequest "2" (ServerStateSendPort sp2)
        join3 = JoinRequest "3" (ServerStateSendPort sp3)

    (JoinRequestResult (Right (JoinAccepted xs1))) <- joinRequest sPid join1
    (JoinRequestResult (Right (JoinAccepted xs2))) <- joinRequest sPid join2
    (JoinRequestResult (Left  (JoinError    err))) <- joinRequest sPid join3

    P.liftIO $ xs1 @?= []
    P.liftIO $ xs2 @?= ["1"]
    P.liftIO $ err @=? "error message"
    return ()

-- Create a server in a computation, pass it to the test and then shutdown the server
withServerClients
  :: IO (LocalServer, TMVar [JoinRequestResult [Nickname]])
  -> (LocalServer -> [JoinRequestResult [Nickname]] -> Assertion)
  -> Node.LocalNode
  -> Assertion
withServerClients mkServer test n = do
  (server, v) <- mkServer
  (Right _)   <- atomically $ readTMVar (pidApiServer server)
  results     <- atomically $ readTMVar v
  test server results
  exitServer server
 where
  exitServer s = do
    Node.runProcess n $ exitProc s "" >> P.unregister testSession

-- Create a server in a computation, pass it to the test and then shutdown the server
withServer
  :: IO LocalServer -> (LocalServer -> Assertion) -> Node.LocalNode -> Assertion
withServer mkServer test n = do
  server    <- mkServer
  (Right _) <- atomically $ readTMVar (pidApiServer server)
  test server
  exitServer server
 where
  exitServer s = do
    Node.runProcess n $ exitProc s "" >> P.unregister testSession

-- TODO test:
-- 1. servers that run simulations themselves
-- 2. test stateUpdate sending
-- joining leaving simulations in progress
-- send state to clients
-- return initial state of world on join
-- # library users can decide whether clients can join
-- servers need to be handle a varying amount of connected clients
-- clients do not need to determine the destination of messages (responsibility of servers)
-- users of the library need to be able to add new message types
-- state Updates are generic, users of the library can decide what types of data should be transmitted and how network data is processed
-- servers need way to run simulations themselves

