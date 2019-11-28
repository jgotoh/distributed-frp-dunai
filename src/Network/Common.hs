{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}

module Network.Common
  ( HostName
  , ServiceName
  , SessionName
  , Nickname
  , ServerStateSendPort(..)
  , ServerStateReceivePort(..)
  , JoinRequest(..)
  , JoinRequestResult(..)
  , JoinAccepted(..)
  , JoinError(..)
  , createLocalNode
  , initializeNode
  , Binary
  , Generic
  , Typeable
  , CommandRate
  , ServerStateChannel(..)
  , Port
  , ServerAddress
  , serverStateSendPort
  , serverStateReceivePort
  , searchProcessTimeout
  , StateUpdate(..)
  , Node.LocalNode
  )
where

import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import qualified Control.Distributed.Process.Node
                                               as Node
import           Control.Distributed.Process.Serializable
import qualified Network.Transport.TCP         as NT
import qualified Network.Socket                as N
import           Control.Exception.Base         ( IOException )
import           Data.Binary                    ( Binary )
import qualified Network.Transport             as T
import           Network.Socket                 ( HostName
                                                , ServiceName
                                                )
import           Type.Reflection
import           GHC.Generics                   ( Generic )

type Nickname = String
type SessionName = String
type ServerAddress = String
-- Port number
type Port = Int

-- The frequency clients are sending StateUpdates
type CommandRate = Time.TimeInterval

-- Channel used by the server to send StateUpdates to clients
data ServerStateChannel a = ServerStateChannel (ServerStateSendPort a) (ServerStateReceivePort a)

-- Port used by a server to send StateUpdates to a client. Server -[state]-> Client
newtype ServerStateSendPort a = ServerStateSendPort (P.SendPort (StateUpdate a))
  deriving (Generic, Show, Typeable, Eq)
instance Serializable a => Binary (ServerStateSendPort a)

-- Port used by a client to receive StateUpdates from a server. Server -[state]-> Client
newtype ServerStateReceivePort a = ServerStateReceivePort (P.ReceivePort (StateUpdate a))
  deriving (Generic)

data JoinRequest a = JoinRequest Nickname (ServerStateSendPort a)
  deriving (Generic, Show, Typeable, Eq)
instance Serializable a => Binary (JoinRequest a)

newtype JoinRequestResult a = JoinRequestResult (Either JoinError (JoinAccepted a))
  deriving (Generic, Show, Typeable, Eq)
instance Serializable a => Binary (JoinRequestResult a)

newtype JoinError = JoinError String
  deriving (Generic, Show, Typeable, Eq)
instance Binary JoinError

newtype JoinAccepted a = JoinAccepted a
  deriving (Generic, Show, Typeable, Eq)
instance Serializable a => Binary (JoinAccepted a)

data StateUpdate a = StateUpdate P.ProcessId a
  deriving (Generic, Show, Typeable, Eq)
instance Binary a => Binary (StateUpdate a)

serverStateSendPort :: ServerStateSendPort a -> P.SendPort (StateUpdate a)
serverStateSendPort sp = case sp of
  ServerStateSendPort p -> p

serverStateReceivePort
  :: ServerStateReceivePort a -> P.ReceivePort (StateUpdate a)
serverStateReceivePort rp = case rp of
  ServerStateReceivePort p -> p

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

initializeNode
  :: N.HostName
  -> Port
  -> IO (Either IOException (Node.LocalNode, T.Transport))
initializeNode ip port = do
  -- TODO is Bifunctor.second usable here?
  t <- NT.createTransport (NT.defaultTCPAddr ip (show port)) NT.defaultTCPParameters
  case t of
    Left  l -> return $ Left l
    Right r -> do
      n <- createLocalNode r
      return $ Right (n, r)

