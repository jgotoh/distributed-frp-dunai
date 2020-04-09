{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ServerTest
  ( serverTests
  )
where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception.Base         ( IOException )
import           Control.Monad
import qualified Data.Set as Set
import           Network.Common
import           Network.Server
import qualified Network.Socket                as N
import qualified Network.Transport.TCP         as NT
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Node
                                               as Node
import qualified Control.Distributed.Process.Extras.SystemLog
                                               as Log
import qualified Network.Transport             as T

data TestMessage = Ping | Pong
  deriving (Generic, Show, Typeable, Eq)
instance Binary TestMessage

type TestUpdate = UpdatePacket TestMessage

testIp :: HostName
testIp = "localhost"

-- requesting port 0 tells the system to automatically find an unused port
testPort :: Port
testPort = 0

testSession :: SessionName
testSession = "testSession"

testConfiguration :: Node.LocalNode -> ServerConfiguration TestMessage
testConfiguration n =
  defaultServerConfig n testIp testPort testSession defaultFRPProcessDefinition

serverTests :: TestTree
serverTests = withResource withNode clearNode tests
 where
  withNode = initializeNode testIp testPort >>= \case
    Left  err        -> error $ show err
    Right nt@(n', _) -> do
      Node.runProcess n' $ do
        _ <- Log.systemLog (P.liftIO . print) (return ()) Log.Debug return
        return ()
      return nt
  clearNode (n, t) = do
    Node.closeLocalNode n >> T.closeTransport t

tests :: IO (Node.LocalNode, T.Transport) -> TestTree
tests mkNT = testGroup
  "ServerTests"
  [ testCase "startup using default configuration" $ mkNT >>= testDefaultServer
  , testCase "join requests and termination" $ mkNT >>= testJoinRequests
  , testCase "receiving client updates" $ mkNT >>= testClientUpdates
  , testCase "sending of states" $ mkNT >>= testSendState
  , testCase "write UpdatePackets with writeState" $ mkNT >>= testWriteState
  , testCase "read Commands with receiveCommands" $ mkNT >>= testReceiveCommands
  ]

-- Tests whether CommandPackets sent by clientUpdate are correctly added to the readQueue of a server
testClientUpdates :: (Node.LocalNode, T.Transport) -> Assertion
testClientUpdates (n, _) = withServer
  (startServerProcess (testConfiguration n) :: IO
      (LocalServer TestMessage TestMessage)
  )
  test
  n
 where
  nick1 = "1"
  test server = Node.runProcess n $ do

    (Right sPid)   <- P.liftIO . atomically . readTMVar $ pidApiServer server

    -- testProcess contains a typed channel which basically mocks a client
    (sp1v, sp1pid) <- P.liftIO $ testProcess n
    sp1            <- P.liftIO . atomically $ readTMVar sp1v

    let ssp1   = ServerStateSendPort sp1
        join1  = JoinRequest nick1 ssp1
        update = CommandPacket sp1pid 0 Ping

    -- Send CommandPacket without first connecting to the server
    -- the Server should ignore it
    clientUpdate sPid update
    P.liftIO $ threadDelay 1000000
    updates <- P.liftIO . atomically . flushTQueue $ readQueue server
    P.liftIO $ [] @=? updates

    -- Connect/ join, then send again
    _ <- joinRequest sPid join1
    clientUpdate sPid update
    P.liftIO $ threadDelay 1000000
    updates' <- P.liftIO . atomically . flushTQueue $ readQueue server
    P.liftIO $ [update] @=? updates'

    clientUpdate sPid update
    clientUpdate sPid update
    P.liftIO $ threadDelay 1000000
    updates'' <- P.liftIO . atomically . flushTQueue $ readQueue server
    P.liftIO $ [update, update] @=? updates''

-- Test whether writing into the sendVar of a Server actually sends data.
testSendState :: (Node.LocalNode, T.Transport) -> Assertion
testSendState (n, _) = withServer
  (startServerProcess (testConfiguration n) :: IO
      (LocalServer TestMessage TestMessage)
  )
  test
  n
 where
  test server = Node.runProcess n $ do

    (Right sPid)   <- P.liftIO . atomically . readTMVar $ pidApiServer server

    -- create a channel which will receive states from the server
    (sp1, rp1) <-
      P.newChan :: P.Process (P.SendPort TestUpdate, P.ReceivePort TestUpdate)
    (sp2, rp2) <-
      P.newChan :: P.Process (P.SendPort TestUpdate, P.ReceivePort TestUpdate)
    P.linkPort sp1
    P.linkPort sp2

    let sendVar' = (sendVar server)

    let tm1 = UpdatePacket sPid 0 Ping
    -- write a message to sendVar
    writeV sendVar' [(sp1, tm1)]

    -- test: rp1 has tm1, sendVar is empty
    tm1' <- P.receiveChan rp1
    P.liftIO $ tm1' @?= tm1
    isNull <- P.liftIO . atomically $ isEmptyTMVar sendVar'
    P.liftIO $ assertBool "sendVar should be empty" isNull

    -- write multiple messages
    let tm2 = UpdatePacket sPid 1 Pong
    let tm3 = UpdatePacket sPid 2 Pong
    writeV sendVar' [(sp1, tm2), (sp2, tm3)]

    tm2' <- P.receiveChan rp1
    tm3' <- P.receiveChan rp2
    P.liftIO $ tm2' @?= tm2
    P.liftIO $ tm3' @?= tm3
    isNull' <- P.liftIO . atomically $ isEmptyTMVar sendVar'
    P.liftIO $ assertBool "sendVar should be empty" isNull'


writeV :: TMVar [(P.SendPort TestUpdate, TestUpdate)] -> [(P.SendPort TestUpdate, TestUpdate)] -> P.Process ()
writeV var v = P.liftIO $ atomically $ putTMVar var v

-- Tests whether a server using the default configuration starts correctly.
testDefaultServer :: (Node.LocalNode, T.Transport) -> Assertion
testDefaultServer (n, _) = withServer
  (startServerProcess (testConfiguration n) :: IO
      (LocalServer TestMessage TestMessage)
  )
  test
  n
 where
  test server = Node.runProcess n $ do

    let serverPid = pidServer server
        vPid      = pidApiServer server

    (Right apiPid) <- P.liftIO $ atomically $ readTMVar vPid
    main           <- isProcessAlive serverPid
    api            <- isProcessAlive apiPid

    P.liftIO $ main @? "main process is not active"
    P.liftIO $ api @? "api process is not active"

-- Tests handling of JoinRequests and monitoring of ServerStateSendPorts
-- Tests whether setting a custom JoinRequestHandler works, afterwards connected clients quit to test whether a server's state is correctly updated.
testJoinRequests :: (Node.LocalNode, T.Transport) -> Assertion
testJoinRequests (n, _) = withServer
  (startServerProcess cfg :: IO (LocalServer TestMessage TestMessage))
  (test n)
  n
 where
  cfg :: ServerConfiguration TestMessage
  cfg = (testConfiguration n) { joinConfig = twoClients }

  -- allow two clients to connect
  twoClients xs _ = return $ JoinRequestResult $ if length xs < 2
    then Right $ JoinAccepted $ nicks xs
    else Left $ JoinError errorMsg
  nicks    = map nameClient
  errorMsg = "error message"
  nick1    = "1"
  nick2    = "2"
  nick3    = "3"
  test node server = Node.runProcess node $ do

    (Right sPid)   <- P.liftIO . atomically . readTMVar $ pidApiServer server

    -- Create 3 Processes with SendPorts to connect to server
    (sp1v, sp1pid) <- P.liftIO $ testProcess n
    (sp2v, sp2pid) <- P.liftIO $ testProcess n
    (sp3v, _     ) <- P.liftIO $ testProcess n

    sp1            <- P.liftIO $ atomically $ readTMVar sp1v
    sp2            <- P.liftIO $ atomically $ readTMVar sp2v
    sp3            <- P.liftIO $ atomically $ readTMVar sp3v

    let ssp1  = ServerStateSendPort sp1
        ssp2  = ServerStateSendPort sp2
        ssp3  = ServerStateSendPort sp3
        join1 = JoinRequest nick1 ssp1
        join2 = JoinRequest nick2 ssp2
        join3 = JoinRequest nick3 ssp3

    -- first two requests are accepted, third is rejected
    (JoinRequestResult (Right (JoinAccepted xs1))) <- joinRequest sPid join1
    (JoinRequestResult (Right (JoinAccepted xs2))) <- joinRequest sPid join2
    (JoinRequestResult (Left  (JoinError    err))) <- joinRequest sPid join3

    P.liftIO $ xs1 @?= []
    P.liftIO $ xs2 @?= [nick1]
    P.liftIO $ err @?= errorMsg

    -- Check server's state
    -- state should be compared for equality independently of its order due to concurrent creation
    state <- P.liftIO $ getState server >>= return . Set.fromList

    let expectedClient1 = Client nick1 ssp1
        expectedClient2 = Client nick2 ssp2
        expectedSet = Set.fromList [expectedClient2, expectedClient1]

    P.liftIO $ expectedSet @=? state

    -- then remove clients, and test getState again
    P.kill sp1pid "removing client 1"
    -- the notification the server receives when a client quits is delivered asynchronously, so we first wait an arbitrary amount of time
    P.liftIO $ threadDelay 1000000
    state' <- P.liftIO $ getState server
    let expectedState' = [expectedClient2]
    P.liftIO $ expectedState' @=? state'

    P.kill sp2pid "removing client 2"
    P.liftIO $ threadDelay 1000000
    state'' <- P.liftIO $ getState server
    P.liftIO $ [] @=? state''

-- Starts a process that creates a channel for state exchanges and checks forever for incoming messages
testProcess :: Node.LocalNode -> IO (TMVar (P.SendPort TestUpdate), P.ProcessId)
testProcess n = do
  v   <- newEmptyTMVarIO
  pid <- Node.forkProcess n $ do
    (sp, rp) <-
      P.newChan :: P.Process (P.SendPort TestUpdate, P.ReceivePort TestUpdate)
    P.liftIO . atomically $ putTMVar v sp
    P.linkPort sp
    forever $ P.receiveChan rp
  return (v, pid)

-- Create a server in a computation, pass it to the test and then shutdown the server
withServer
  :: IO (LocalServer a b)
  -> (LocalServer a b -> Assertion)
  -> Node.LocalNode
  -> Assertion
withServer mkServer test n = do
  server    <- mkServer
  (Right _) <- atomically $ readTMVar (pidApiServer server)
  test server
  exitServer server
 where
  exitServer s =
    Node.runProcess n
      $  exitProc s "exiting server via withServer"
      >> P.unregister testSession

-- Convenience function to create a node
initializeNode
  :: N.HostName -> Port -> IO (Either IOException (Node.LocalNode, T.Transport))
initializeNode ip port = do
  t <- NT.createTransport (NT.defaultTCPAddr ip (show port))
                          NT.defaultTCPParameters
  case t of
    Left  l -> return $ Left l
    Right r -> do
      n <- createLocalNode r
      return $ Right (n, r)

testWriteState :: (Node.LocalNode, T.Transport) -> Assertion
testWriteState (node, _ ) = Node.runProcess node $ do
  var <- P.liftIO $ newEmptyTMVarIO

  (sp, _) <-
    P.newChan :: P.Process (P.SendPort (UpdatePacket Int), P.ReceivePort (UpdatePacket Int))
  pid <- P.getSelfPid

  -- writeState actually writes
  let up x = UpdatePacket pid 10 x
      x1 = (sp, up 1)
      x2 = (sp, up 2)
  P.liftIO $ writeState var [x1, x2]

  results <- P.liftIO . atomically $ readTMVar var
  P.liftIO $ results @?= [x1, x2]

  -- writeState replaces an existing value
  let x3 = (sp, up 3)
      x4 = (sp, up 4)
  P.liftIO $ writeState var [x3, x4]

  results' <- P.liftIO . atomically $ readTMVar var
  P.liftIO $ results' @?= [x3, x4]

testReceiveCommands :: (Node.LocalNode, T.Transport) -> Assertion
testReceiveCommands (node, _ ) = Node.runProcess node $ do
  q <- P.liftIO $ newTQueueIO

  let x1 = 1
      x2 = 2

  P.liftIO . atomically $ writeTQueue q x1

  rs1 <- P.liftIO $ receiveCommands q
  P.liftIO $ rs1 @?= [x1]
  isNull <- P.liftIO . atomically $ isEmptyTQueue q
  P.liftIO $ assertBool "queue should be empty" isNull

  P.liftIO . atomically $ writeTQueue q x1
  P.liftIO . atomically $ writeTQueue q x2

  rs2 <- P.liftIO $ receiveCommands q
  P.liftIO $ rs2 @?= [x1, x2]
  isNull' <- P.liftIO . atomically $ isEmptyTQueue q
  P.liftIO $ assertBool "queue should be empty" isNull'

