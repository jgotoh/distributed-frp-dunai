{-# LANGUAGE DeriveGeneric #-}

module ServerTest where

import Network.Common
import Network.AuthoritativeServer
import Test.Tasty
import Test.Tasty.HUnit

data TestMessage = Ping | Pong
  deriving (Generic, Show, Typeable)
instance Binary TestMessage

testIp :: HostName
testIp = "localhost"

testPort :: ServiceName
testPort = "3000"

testSession :: SessionName
testSession = "testSession"

testServerProcessDefinition :: ServerProcessDefinition TestMessage
testServerProcessDefinition = undefined

serverTests :: TestTree
serverTests = testGroup "ServerTests" [startServerTest]

--startServerTest = testCase "startIt" $ [1, 2, 3] `compare` [1,2] @?= GT

startServerTest :: TestTree
startServerTest = testCase "testing server startup" $ do
  --cfg <- undefined
  return undefined
