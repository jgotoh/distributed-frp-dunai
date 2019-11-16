module Network.AuthoritativeServer
  ( clientUpdate
  , joinRequest
  , ServerProcessDefinition
  , defaultTCPServerConfig
  , startServerProcess
  )
where

import           Network.Common          hiding ( Client )
import           Network.Internal.ServerCommon
import           Control.Concurrent
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.SystemLog
                                               as Log
-- import qualified Control.Distributed.Process.Extras.Time
                                               -- as Time
import           Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import qualified Control.Distributed.Process.Node
                                               as Node
import           Control.Monad
import qualified Network.Socket                as N
import qualified Network.Transport.TCP         as NT

-- client facing API

-- Sends a Command packet
clientUpdate
  :: (Addressable a, Binary m, Typeable m)
  => a
  -> CommandPacket m
  -> P.Process ()
clientUpdate = MP.cast

-- server setup

defaultTCPServerConfig
  :: (Binary a, Typeable a)
  => N.HostName
  -> N.ServiceName
  -> SessionName
  -> ServerProcessDefinition a
  -> ServerConfiguration IO a
defaultTCPServerConfig ip port name def = ServerConfiguration
  { hostConfig              = ip
  , portConfig              = port
  , nameConfig              = name
  , transportConfig         = createTCPTransport
  , processDefinitionConfig = def
  }
 where
  createTCPTransport =
    NT.createTransport (NT.defaultTCPAddr ip port) NT.defaultTCPParameters

startServerProcess
  :: (Binary a, Typeable a) => ServerConfiguration IO a -> IO Server
startServerProcess cfg = do
  eTransport <- transportConfig cfg
  case eTransport of
    Left  e         -> error $ show e
    Right transport -> do
      node <- createLocalNode transport
      pid  <- Node.forkProcess node $ do
        _ <- Log.systemLog (P.liftIO . print) (return ()) Log.Debug return

        let serverAddress = P.nodeAddress $ Node.localNodeId node
        pid <- P.spawnLocal $ serverProcess (processDefinitionConfig cfg)

        P.link pid

        P.liftIO
          $  print
          $  "Server starts at: "
          ++ show serverAddress
          ++ " ProcessId: "
          ++ show pid
        P.register (nameConfig cfg) pid
        P.liftIO $ forever $ threadDelay 16
      return $ Server pid

