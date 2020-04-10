{-# LANGUAGE LambdaCase #-}

module ClientTest
  ( clientTests
  )
where

import           Network.Client
import           Network.Server
import           Network.Common
import           ServerTest
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.SystemLog
                                               as Log
import qualified Control.Distributed.Process.Node
                                               as Node
import qualified Network.Transport             as T
import           Test.Tasty
import           Test.Tasty.HUnit

testIp :: HostName
testIp = "localhost"

testPort :: Port
testPort = 0

clientTests :: TestTree
clientTests = withResource withNode clearNode tests
 where
  withNode = initializeNode testIp testPort >>= \case
    Left  err        -> error $ show err
    Right nt@(n', _) -> do
      Node.runProcess n' $ do
        _ <- Log.systemLog (P.liftIO . print) (return ()) Log.Debug return
        return ()
      return nt
  clearNode (n, t) = Node.closeLocalNode n >> T.closeTransport t

tests :: IO (Node.LocalNode, T.Transport) -> TestTree
tests mkNT = testGroup
  "ClientTests"
  [ testCase "test receiving updates" $ mkNT >>= testReceiveUpdates
  , testCase "test sending of commands" $ mkNT >>= testSendCommands
  , testCase "test joinRequest" $ mkNT >>= testJoinRequest
  , testCase "test notification on termination of servers"
  $   mkNT
  >>= testTermination
  , testCase "test writing commands with writeCommand"
  $   mkNT
  >>= testWriteCommand
  , testCase "test reading states with receiveState" $ mkNT >>= testReceiveState
  ]

-- tests whether states received via a typed channel are correctly added a the readVar of a Client. Also tests whether existing state in this variable will be replaced upon new states.
testReceiveUpdates :: (LocalNode, T.Transport) -> IO ()
testReceiveUpdates (n, _) = withServer
  (startServerProcess (testConfiguration n) :: IO
      (LocalServer TestMessage TestMessage)
  )
  test
  n
 where
  test server = Node.runProcess n $ do

    (Right sPid) <- P.liftIO . atomically . readTMVar $ pidApiServer server
    (client, joinResultVar) <- startTestClient n server "1"

    -- test whether connection between client and server was set up correctly
    joinResult <- P.liftIO . atomically $ takeTMVar joinResultVar
    P.liftIO $ joinResult @?= JoinRequestResult (Right (JoinAccepted []))

    -- write an UpdatePacket into LocalServer's sendVar
    let readVar' = readVar client
        tm1      = UpdatePacket sPid 0 Pong

    -- get the SendPort of the Client
    [c] <- P.liftIO $ getState server

    let sp = serverStateSendPort $ serverStateClient c
    P.liftIO $ writeState (Network.Server.sendVar server) [(sp, tm1)]

    P.liftIO $ print "pre read 1"
    -- get value of readVar without emptying it
    update1 <- P.liftIO . atomically $ readTMVar readVar'

    P.liftIO $ update1 @?= tm1

    -- send another UpdatePacket, then the value of readVar should be replaced
    let tm2 = UpdatePacket sPid 3 Ping
    P.liftIO $ writeState (Network.Server.sendVar server) [(sp, tm2)]

    _       <- P.liftIO $ threadDelay 1000000
    update2 <- P.liftIO . atomically $ readTMVar readVar'

    P.liftIO $ update2 @?= tm2


-- test whether writing into the sendVar of a client sends a CommandPacket
testSendCommands :: (LocalNode, T.Transport) -> IO ()
testSendCommands (n, _) = withServer
  (startServerProcess (testConfiguration n) :: IO
      (LocalServer TestMessage TestMessage)
  )
  test
  n
 where
  test server = Node.runProcess n $ do

    (client, _) <- startTestClient n server "1"

    -- write into sendVar
    let sendVar' = Network.Client.sendVar client
        tm1      = CommandPacket (Network.Client.pidClient client) 0 Pong
    P.liftIO . atomically $ putTMVar sendVar' tm1

    P.liftIO $ threadDelay 1000000

    -- check server's readQueue
    cs <- P.liftIO $ receiveCommands (readQueue server)

    P.liftIO $ cs @?= [tm1]

-- tests whether joinRequests work. Start Server, then let 3 clients join.
testJoinRequest :: (LocalNode, T.Transport) -> IO ()
testJoinRequest (n, _) = withServer
  (startServerProcess (testConfiguration n) { joinConfig = twoClients } :: IO
      (LocalServer TestMessage TestMessage)
  )
  test
  n
 where
  -- allow two clients to connect
  twoClients xs _ = return $ JoinRequestResult $ if length xs < 2
    then Right $ JoinAccepted []
    else Left $ JoinError ""
  test server = Node.runProcess n $ do

    (_, joinResultVar1)                          <- startTestClient n server "1"
    (_, joinResultVar2)                          <- startTestClient n server "2"
    (_, joinResultVar3)                          <- startTestClient n server "3"

    -- check joinResults
    (JoinRequestResult (Right (JoinAccepted _))) <-
      P.liftIO . atomically $ takeTMVar joinResultVar1
    (JoinRequestResult (Right (JoinAccepted _))) <-
      P.liftIO . atomically $ takeTMVar joinResultVar2
    (JoinRequestResult (Left (JoinError _))) <-
      P.liftIO . atomically $ takeTMVar joinResultVar3

    return ()

-- tests whether a client process quits when a connected server terminates.
testTermination :: (LocalNode, T.Transport) -> IO ()
testTermination (n, _) = Node.runProcess n $ do

    -- because we will terminate the server prematurely, we cannot use withServer here
  let mkServer =
        startServerProcess (testConfiguration n) :: IO
            (LocalServer TestMessage TestMessage)
  server                  <- P.liftIO mkServer

  (client, joinResultVar) <- startTestClient n server "1"

  joinResult              <- P.liftIO . atomically $ takeTMVar joinResultVar
  P.liftIO $ joinResult @?= JoinRequestResult (Right (JoinAccepted []))

  -- client process still exists
  (Just  _        ) <- P.getProcessInfo (Network.Client.pidClient client)

  -- kill server
  (Right serverPid) <- P.liftIO . atomically . readTMVar $ pidApiServer server
  P.kill serverPid "terminate is expected"

  -- client should terminate now, which means getProcessInfo will return Nothing
  _    <- P.liftIO $ threadDelay 1000000
  info <- P.getProcessInfo (Network.Client.pidClient client)

  P.liftIO $ info @?= Nothing


-- tests whether writeCommand correctly puts a command into a TMVar.
testWriteCommand :: (LocalNode, T.Transport) -> IO ()
testWriteCommand (n, _) = Node.runProcess n $ do

  pid <- P.getSelfPid
  v   <- P.liftIO newEmptyTMVarIO

  let c1 = CommandPacket pid 1 "c"

  P.liftIO $ writeCommand v c1

  c1' <- P.liftIO . atomically $ takeTMVar v

  P.liftIO $ c1' @?= c1

-- tests whether receiveState returns content of a TMVar and empties it. Also tests whether it returns nothing if the TMVar is empty.
testReceiveState :: (LocalNode, T.Transport) -> IO ()
testReceiveState (n, _) = Node.runProcess n $ do

  pid <- P.getSelfPid
  v   <- P.liftIO newEmptyTMVarIO

  s0  <- P.liftIO $ receiveState v

  P.liftIO $ s0 @?= Nothing

  let s1 = UpdatePacket pid 1 "u"

  P.liftIO . atomically $ putTMVar v s1

  s1' <- P.liftIO $ receiveState v
  P.liftIO $ s1' @?= Just s1

  s1'' <- P.liftIO $ receiveState v
  P.liftIO $ s1'' @?= Nothing

-- Start a client, join a local server
startTestClient
  :: LocalNode
  -> LocalServer TestMessage TestMessage
  -> String
  -> P.Process
       ( LocalClient TestMessage TestMessage
       , TMVar (JoinRequestResult [Nickname])
       )
startTestClient n server nick = do
  (Right sPid) <- P.liftIO . atomically . readTMVar $ pidApiServer server

  let remoteServer = Server sPid
      createChan =
        createServerStateChannel :: P.Process (ServerStateChannel TestMessage)

  P.liftIO $ startClientProcess n remoteServer nick createChan :: P.Process
      ( LocalClient TestMessage TestMessage
      , TMVar (JoinRequestResult [Nickname])
      )

