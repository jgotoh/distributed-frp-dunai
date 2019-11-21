{-# LANGUAGE DeriveGeneric #-}

module Network.Internal.ServerCommon
  ( ServerProcessDefinition
  , Client(..)
  , ServerState
  , joinRequest
  , ServerConfiguration(..)
  , serverProcess
  , handleJoinRequest
  , handleMonitorNotification
  , logTimeout
  , logShutdown
  , withoutClient
  , withoutClient'
  , broadcastUpdate
  )
where

import Control.Monad
import           Control.Exception
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import qualified Network.Socket                as N
import           Network.Common          hiding ( Client )
import qualified Network.Transport             as T
import           GHC.Generics                   ( Generic )
import           Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import qualified Control.Distributed.Process.Node
                                               as Node

type ServerProcessDefinition a = MP.ProcessDefinition (ServerState a)

data ServerConfiguration m a = ServerConfiguration
  { nodeConfig :: Node.LocalNode
  , hostConfig :: N.HostName
  , portConfig :: N.ServiceName
  , nameConfig :: SessionName
  , transportConfig :: m (Either IOException T.Transport)
  , processDefinitionConfig :: ServerProcessDefinition a
  , joinConfig :: ServerState a -> JoinRequest a -> P.Process (JoinRequestResult [Nickname])
  }

data Client a = Client
  { nameClient :: Nickname
  , serverStateClient :: ServerStateSendPort a
  }
  deriving (Generic, Typeable)

instance Show (Client a) where
  show c = "Client " ++ nameClient c ++ "," ++ show (serverStateClient c)

type ServerState a = [Client a]

-- Sends a JoinRequest and returns the result
joinRequest
  :: (Addressable a, Binary m, Typeable m)
  => a
  -> JoinRequest m
  -> P.Process (JoinRequestResult [Nickname])
joinRequest = MP.call

serverProcess
  :: (Binary a, Typeable a) => ServerProcessDefinition a -> ServerState a -> P.Process ()
serverProcess def s0 = MP.serve () initHandler def
  where initHandler _ = return (MP.InitOk s0 Time.NoDelay)

-- TODO pass in function that decides whether request is accepted
handleJoinRequest
  :: (Binary a, Typeable a)
  => MP.CallHandler
       (ServerState a)
       (JoinRequest a)
       (JoinRequestResult [Nickname])
handleJoinRequest s (JoinRequest nick serverStatePort) = do
  P.liftIO $ print $ "new JoinRequest" ++ show serverStatePort
  _ <- P.monitorPort (serverStateSendPort serverStatePort)
  let s' = client : s
  P.liftIO $ print $ "JoinRequest:: " ++ show s'
  MP.reply (JoinRequestResult $ Right (JoinAccepted nicks)) s'
 where
  client = Client nick serverStatePort
  nicks  = map nameClient s

handleMonitorNotification
  :: MP.ActionHandler (ServerState a) P.PortMonitorNotification
handleMonitorNotification s (P.PortMonitorNotification ref port reason) = do
  P.liftIO
    $  print
    $  "client process died: "
    ++ show port
    ++ " reason: "
    ++ show reason
  P.liftIO $ print $ "new state: " ++ show s'

  P.unmonitor ref
  MP.continue s'
  where
    s' = withoutClient' port s

-- TODO summarize withoutClient[']. maybe via contramap
withoutClient' :: P.SendPortId -> ServerState a -> ServerState a
withoutClient' portId =
  filter (not <$> hasProcessId (P.sendPortProcessId portId))

withoutClient :: P.ProcessId -> ServerState a -> ServerState a
withoutClient pid = filter (not <$> hasIdentification pid)

--generalizedHasId :: Eq b => b -> (Client a -> b) -> Client a -> Bool
--generalizedHasId b f c = b == f c

hasProcessId :: P.ProcessId -> Client a -> Bool
hasProcessId id' (Network.Internal.ServerCommon.Client _ port) =
  P.sendPortProcessId (P.sendPortId (serverStateSendPort port)) == id'

hasIdentification :: P.ProcessId -> Client a -> Bool
hasIdentification pid c = pid == P.sendPortProcessId
  (P.sendPortId $ serverStateSendPort $ serverStateClient c)

logTimeout :: s -> Time.Delay -> MP.Action s
logTimeout s delay = do
  when (delay /= Time.NoDelay) $ P.liftIO $ print $ "logTimeout: " ++ show delay
  MP.continue s

logShutdown :: MP.ExitState s -> ExitReason -> P.Process ()
logShutdown _ reason = P.liftIO $ print $ "logShutdown: " ++ show reason

broadcastUpdate
  :: (Binary a, Typeable a) => StateUpdate a -> [Client a] -> P.Process ()
broadcastUpdate msg clients = forM_ clients (serverUpdate msg)

serverUpdate
  :: (Binary a, Typeable a) => StateUpdate a -> Client a -> P.Process ()
serverUpdate m c = P.sendChan (serverStateSendPort (serverStateClient c)) m
