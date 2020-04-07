-- | This module contains type definitions used by servers. TODO move all functions into Network.Server, and all remaining Type Definitions from Network.Server here
{-# LANGUAGE DeriveGeneric #-}

module Network.Internal.ServerCommon
  ( ServerProcessDefinition
  , Client(..)
  , ServerState
  , joinRequest
  , ServerConfiguration(..)
  , apiProcess
  , logTimeout
  , logShutdown
  , withoutClient
  , withoutClient'
  , broadcastUpdate
  , addApiHandler
  , addInfoHandler
  , pidClient
  , addExternHandler
  , Node.LocalNode
  , Resolvable(..)
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

-- | Alias for Cloud Haskell's 'ProcessDefinition', specialized to 'ServerState'
type ServerProcessDefinition a = MP.ProcessDefinition (ServerState a)

-- | The configuration used to start a server using 'startServerProcess'
data ServerConfiguration a = ServerConfiguration
  { -- | The Node to start the server on
    nodeConfig :: Node.LocalNode
    -- | The 'HostName' where the server runs on
  , hostConfig :: N.HostName
    -- | The 'Port' where the server runs on
  , portConfig :: Port
    -- | The 'SessionName', the API will be added to Cloud Haskell's process registry under this name. See 'register'
  , nameConfig :: SessionName
    -- | Definition of the API
  , processDefinitionConfig :: ServerProcessDefinition a
    -- | Executed when a JoinRequest is processed
  , joinConfig :: ServerState a -> JoinRequest a -> P.Process (JoinRequestResult [Nickname])
  }

-- | A client as used in 'ServerState'
data Client a = Client
  { -- | Name of the client
    nameClient :: Nickname
    -- | 'SendPort' that is used to transmit an application's states
  , serverStateClient :: ServerStateSendPort a
  }
  deriving (Generic, Typeable, Eq)

instance Show (Client a) where
  show c = "Client " ++ nameClient c ++ "," ++ show (serverStateClient c)

instance Addressable (Client a)

instance Resolvable (Client a) where
  resolve a = case a of
    Client _ sp -> resolve sp
  unresolvableMessage a = "Client could not be resolved: " ++ show a

instance Routable (Client a) where
  sendTo s m = resolve s >>= maybe (error $ unresolvableMessage s) (`P.send` m)
  unsafeSendTo s m =
    resolve s >>= maybe (error $ unresolvableMessage s) (`P.unsafeSend` m)

-- | State of a server is a list of connected clients
type ServerState a = [Client a]

-- | Sends a 'JoinRequest' and returns the result using 'MP.call'
joinRequest
  :: (Addressable a, Binary m, Typeable m)
  => a
  -> JoinRequest m
  -> P.Process (JoinRequestResult [Nickname])
joinRequest = MP.call

-- | Call to 'serve' using the supplied 'ServerProcessDefinition'. 's0' is an initial state, contained clients will be monitored immediately
apiProcess
  :: (Binary a, Typeable a)
  => ServerProcessDefinition a
  -> ServerState a
  -> P.Process ()
apiProcess def s0 = MP.serve () initHandler def
 where
  initHandler _ = do
    forM_ (serverStateSendPort . serverStateClient <$> s0) P.monitorPort
    return (MP.InitOk s0 Time.NoDelay)

-- | Removes a 'Client' from a 'ServerState' identified by the supplied 'SendPortId'
withoutClient' :: P.SendPortId -> ServerState a -> ServerState a
withoutClient' portId =
  filter (not <$> hasProcessId (P.sendPortProcessId portId))

-- | Removes a 'Client' from a 'ServerState' identified by the supplied 'ProcessId'
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

-- | Returns the 'ProcessId' of a 'Client'
pidClient :: Client a -> P.ProcessId
pidClient =
  P.sendPortProcessId . P.sendPortId . serverStateSendPort . serverStateClient

-- | Logs Timeouts using 'print'
logTimeout :: s -> Time.Delay -> MP.Action s
logTimeout s delay = do
  when (delay /= Time.NoDelay) $ P.liftIO $ print $ "logTimeout: " ++ show delay
  MP.continue s

-- | Logs Shutdowns using 'print'
logShutdown :: MP.ExitState s -> ExitReason -> P.Process ()
logShutdown _ reason = P.liftIO $ print $ "logShutdown: " ++ show reason

-- | Send an 'UpdatePacket' to a list of clients
broadcastUpdate
  :: (Binary a, Typeable a) => [Client a] -> UpdatePacket a -> P.Process ()
broadcastUpdate clients msg = forM_ clients (serverUpdate msg)

-- | Send an 'UpdatePacket' to a specific clients
serverUpdate
  :: (Binary a, Typeable a) => UpdatePacket a -> Client a -> P.Process ()
serverUpdate m c = P.sendChan (serverStateSendPort (serverStateClient c)) m

-- | Convenience function to add an 'apiHandler' to a 'ProcessDefinition'
addApiHandler
  :: MP.Dispatcher s -> MP.ProcessDefinition s -> MP.ProcessDefinition s
addApiHandler h def = def { MP.apiHandlers = h : MP.apiHandlers def }

-- | Convenience function to add an 'externHandler' to a 'ProcessDefinition'
addExternHandler
  :: MP.ExternDispatcher s -> MP.ProcessDefinition s -> MP.ProcessDefinition s
addExternHandler h def = def { MP.externHandlers = h : MP.externHandlers def }

-- | Convenience function to add an 'infoHandler' to a 'ProcessDefinition'
addInfoHandler
  :: MP.DeferredDispatcher s -> MP.ProcessDefinition s -> MP.ProcessDefinition s
addInfoHandler h def = def { MP.infoHandlers = h : MP.infoHandlers def }

