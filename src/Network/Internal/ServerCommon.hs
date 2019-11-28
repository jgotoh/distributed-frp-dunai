{-# LANGUAGE DeriveGeneric #-}

module Network.Internal.ServerCommon
  ( ServerProcessDefinition
  , Client(..)
  , ServerState
  , joinRequest
  , ServerConfiguration(..)
  , serverProcess
  , logTimeout
  , logShutdown
  , withoutClient
  , withoutClient'
  , broadcastUpdate
  , addApiHandler
  , addInfoHandler
  , addExternHandler
  )
where

import           Control.Monad
import qualified Control.Distributed.Process.ManagedProcess
                                               as MP
import qualified Network.Socket                as N
import           Network.Common
import           GHC.Generics                   ( Generic )
import           Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import qualified Control.Distributed.Process.Node
                                               as Node
import qualified Control.Distributed.Process.ManagedProcess.Internal.Types
                                               as MP

type ServerProcessDefinition a = MP.ProcessDefinition (ServerState a)

data ServerConfiguration a = ServerConfiguration
  { nodeConfig :: Node.LocalNode
  , hostConfig :: N.HostName
  , portConfig :: N.ServiceName
  , nameConfig :: SessionName
  , processDefinitionConfig :: ServerProcessDefinition a
  , joinConfig :: ServerState a -> JoinRequest a -> P.Process (JoinRequestResult [Nickname])
  }

data Client a = Client
  { nameClient :: Nickname
  , serverStateClient :: ServerStateSendPort a
  }
  deriving (Generic, Typeable, Eq)

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

-- Call to MP.serve, that monitors clients in initial state
serverProcess
  :: (Binary a, Typeable a)
  => ServerProcessDefinition a
  -> ServerState a
  -> P.Process ()
serverProcess def s0 = MP.serve () initHandler def
 where
  initHandler _ = do
    forM_ (serverStateSendPort . serverStateClient <$> s0) P.monitorPort
    return (MP.InitOk s0 Time.NoDelay)

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
  :: (Binary a, Typeable a) => [Client a] -> StateUpdate a -> P.Process ()
broadcastUpdate clients msg = forM_ clients (serverUpdate msg)

serverUpdate
  :: (Binary a, Typeable a) => StateUpdate a -> Client a -> P.Process ()
serverUpdate m c = P.sendChan (serverStateSendPort (serverStateClient c)) m

addApiHandler
  :: MP.Dispatcher s -> MP.ProcessDefinition s -> MP.ProcessDefinition s
addApiHandler h def = def { MP.apiHandlers = h : MP.apiHandlers def }

addExternHandler
  :: MP.ExternDispatcher s -> MP.ProcessDefinition s -> MP.ProcessDefinition s
addExternHandler h def = def { MP.externHandlers = h : MP.externHandlers def }

addInfoHandler
  :: MP.DeferredDispatcher s -> MP.ProcessDefinition s -> MP.ProcessDefinition s
addInfoHandler h def = def { MP.infoHandlers = h : MP.infoHandlers def }
