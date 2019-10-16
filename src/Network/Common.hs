{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}

module Network.Common where

import           Control.Concurrent.STM.TQueue
import qualified Control.Distributed.Process   as P
import           Control.Distributed.Process.Extras
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import qualified Control.Distributed.Process.Node
                                               as Node
import           Control.Distributed.Process.Serializable
import           Data.Binary
import qualified Network.Transport             as T
import           Type.Reflection
import           GHC.Generics                   ( Generic )

data Message = Ping | Pong
  deriving (Generic, Show, Typeable)
instance Binary Message

data UnhandledMessage = Peng
  deriving (Generic, Show, Typeable)
instance Binary UnhandledMessage

type Nickname = String

data Client a = Client { clientPid :: P.ProcessId
                     , sendQueue :: TQueue (StateUpdate a)
                     , readQueue :: TQueue (StateUpdate a)
                     }

newtype Server = Server P.ProcessId
  deriving Show

instance Addressable Server

instance Resolvable Server where
  resolve a = case a of
    Server pid -> return $ Just pid
  unresolvableMessage a = "Server could not be resolved: " ++ show a

instance Routable Server where
  sendTo s m = resolve s >>= maybe (error $ unresolvableMessage s) (`P.send` m)
  unsafeSendTo s m =
    resolve s >>= maybe (error $ unresolvableMessage s) (`P.unsafeSend` m)

-- Channel used by clients to send StateUpdates to a server
data ClientStateChannel a = ClientStateChannel (ClientStateSendPort a) (ClientStateReceivePort a)

-- Port used by a client to send StateUpdates to a server. Client -[state]-> Server
-- TODO remove, client states are sent via MP.call
newtype ClientStateSendPort a = ClientStateSendPort (P.SendPort (StateUpdate a))
  deriving (Generic, Show, Typeable)
instance Serializable a => Binary (ClientStateSendPort a)

-- Port used by a server to receive StateUpdates from a client. Client -[state]-> Server
newtype ClientStateReceivePort a = ClientStateReceivePort (P.ReceivePort (StateUpdate a))
  deriving (Generic)

-- Channel used by the server to send StateUpdates to clients
data ServerStateChannel a = ServerStateChannel (ServerStateSendPort a) (ServerStateReceivePort a)

-- Port used by a server to send StateUpdates to a client. Server -[state]-> Client
newtype ServerStateSendPort a = ServerStateSendPort (P.SendPort (StateUpdate a))
  deriving (Generic, Show, Typeable)
instance Serializable a => Binary (ServerStateSendPort a)

-- Port used by a client to receive StateUpdates from a server. Server -[state]-> Client
newtype ServerStateReceivePort a = ServerStateReceivePort (P.ReceivePort (StateUpdate a))
  deriving (Generic)

data JoinRequest a = JoinRequest Nickname (ServerStateSendPort a)
  deriving (Generic, Show, Typeable)
instance Serializable a => Binary (JoinRequest a)

newtype JoinRequestResult a b = JoinRequestResult (Either JoinError (JoinAccepted a b))
  deriving (Generic, Show, Typeable)
instance (Serializable a, Serializable b) => Binary (JoinRequestResult a b)

newtype JoinError = JoinError String
  deriving (Generic, Show, Typeable)
instance Binary JoinError

data JoinAccepted a b = JoinAccepted (ClientStateSendPort a) b
  deriving (Generic, Show, Typeable)
instance (Serializable a, Serializable b) => Binary (JoinAccepted a b)

data StateUpdate a = StateUpdate P.ProcessId a
  deriving (Generic, Show, Typeable)
instance Binary a => Binary (StateUpdate a)

serverStateSendPort :: ServerStateSendPort a -> P.SendPort (StateUpdate a)
serverStateSendPort sp = case sp of
  ServerStateSendPort p -> p

serverStateReceivePort
  :: ServerStateReceivePort a -> P.ReceivePort (StateUpdate a)
serverStateReceivePort rp = case rp of
  ServerStateReceivePort p -> p

clientStateSendPort :: ClientStateSendPort a -> P.SendPort (StateUpdate a)
clientStateSendPort sp = case sp of
  ClientStateSendPort p -> p

clientStateReceivePort
  :: ClientStateReceivePort a -> P.ReceivePort (StateUpdate a)
clientStateReceivePort rp = case rp of
  ClientStateReceivePort p -> p

-- Searches for a Process under address addr called name. When timeLeft runs out, returns Nothing
searchProcessTimeout
  :: String -> P.NodeId -> Int -> P.Process (Maybe P.ProcessId)
searchProcessTimeout name addr timeLeft
  | timeLeft <= 0 = do
    P.liftIO
      $  print
      $  "searchProcess ended due to timeout. No process "
      ++ name
      ++ " at address "
      ++ show addr
      ++ " could be found"
    return Nothing
  | otherwise = do
    P.whereisRemoteAsync addr name
    reply <- P.expectTimeout timeout
    case reply of
      Just (P.WhereIsReply name' maybeID) -> case maybeID of
        Just pid -> do
          P.liftIO $ print $ "WhereIsReply " ++ name' ++ " pid: " ++ show pid
          return $ Just pid
        Nothing -> searchProcessTimeout name addr (timeLeft - timeoutMS)
      Nothing -> searchProcessTimeout name addr (timeLeft - timeoutMS)
 where
  timeoutMS = 1000
  timeout   = Time.asTimeout $ Time.milliSeconds timeoutMS

createLocalNode :: T.Transport -> IO Node.LocalNode
createLocalNode transport = Node.newLocalNode transport Node.initRemoteTable

