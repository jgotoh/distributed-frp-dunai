-- | This module contains types and functions used by both servers and clients.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}

module Network.Common
  ( HostName
  , ServiceName
  , runProcessIO
  , CommandPacket(..)
  , UpdatePacket(..)
  , SessionName
  , Nickname
  , ServerStateSendPort(..)
  , ServerStateReceivePort(..)
  , JoinRequest(..)
  , JoinRequestResult(..)
  , JoinAccepted(..)
  , JoinError(..)
  , createLocalNode
  , Binary
  , Generic
  , Typeable
  , CommandRate
  , ServerStateChannel(..)
  , Port
  , ServerAddress
  , Resolvable
  , serverStateSendPort
  , serverStateReceivePort
  , searchProcessTimeout
  , Node.LocalNode
  , resolveIO
  , updatePacketData
  , FrameNr
  , HasFrameAssociation(..)
  )
where

import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import           Control.Distributed.Process.Extras.Internal.Types
import qualified Control.Distributed.Process.Node
                                               as Node
import           Control.Distributed.Process.Serializable
import           Numeric.Natural
import           Data.Binary                    ( Binary )
import qualified Network.Transport             as T
import           Network.Socket                 ( HostName
                                                , ServiceName
                                                )
import           Type.Reflection
import           GHC.Generics                   ( Generic )
import           Control.Concurrent.STM.TMVar
import           Control.Monad.STM

-- | Nickname of a client
type Nickname = String

-- | Name of a session hosted by a server which will be added to the Process registry of Cloud Haskell
type SessionName = String

-- | NodeId of a Server
type ServerAddress = String

-- | A Port number
type Port = Int

-- | Number of a Frame
type FrameNr = Natural

-- | Types that are associated with a specific frame
class HasFrameAssociation a where
  -- | returns its FrameNr
  getFrame :: a -> FrameNr

-- | The frequency clients are sending Commandpackets. TODO is unused, delete this
type CommandRate = Time.TimeInterval

-- | Channel used by the server to send UpdatePackets to clients
data ServerStateChannel a = ServerStateChannel (ServerStateSendPort a) (ServerStateReceivePort a)

-- | Port used by a server to send UpdatePackets to a client. Server -[state]-> Client
newtype ServerStateSendPort a = ServerStateSendPort (P.SendPort (UpdatePacket a))
  deriving (Generic, Show, Typeable, Eq)
instance Serializable a => Binary (ServerStateSendPort a)

instance Addressable (ServerStateSendPort a)

instance Resolvable (ServerStateSendPort a) where
  resolve a = case a of
    ServerStateSendPort sp ->
      return . Just . P.sendPortProcessId $ P.sendPortId sp
  unresolvableMessage a =
    "ServerStateSendPort could not be resolved: " ++ show a

instance Routable (ServerStateSendPort a) where
  sendTo s m = resolve s >>= maybe (error $ unresolvableMessage s) (`P.send` m)
  unsafeSendTo s m =
    resolve s >>= maybe (error $ unresolvableMessage s) (`P.unsafeSend` m)

-- | Port used by a client to receive UpdatePackets from a server. Server -[state]-> Client
newtype ServerStateReceivePort a = ServerStateReceivePort (P.ReceivePort (UpdatePacket a))
  deriving (Generic)

-- | A Request to join a server. Must contain a client's nickname and the port used to receive UpdatePackets
data JoinRequest a = JoinRequest Nickname (ServerStateSendPort a)
  deriving (Generic, Show, Typeable, Eq)
instance Serializable a => Binary (JoinRequest a)

-- | The response to JoinRequests
newtype JoinRequestResult a = JoinRequestResult (Either JoinError (JoinAccepted a))
  deriving (Generic, Show, Typeable, Eq)
instance Serializable a => Binary (JoinRequestResult a)

-- | Error Message if a JoinRequest is declined
newtype JoinError = JoinError String
  deriving (Generic, Show, Typeable, Eq)
instance Binary JoinError

-- | Message if a JoinRequest is accepted
newtype JoinAccepted a = JoinAccepted a
  deriving (Generic, Show, Typeable, Eq)
instance Serializable a => Binary (JoinAccepted a)

-- | Commands issued by clients. Are associated with their frame of occurrence and the 'ProcessId' of the emitting client.
data CommandPacket a = CommandPacket P.ProcessId FrameNr a
  deriving (Generic, Show, Typeable)

instance Binary a => Binary (CommandPacket a)

instance HasFrameAssociation (CommandPacket a) where
  getFrame (CommandPacket _ n _) = n

instance Ord (CommandPacket a) where
  (<=) (CommandPacket _ x _) (CommandPacket _ y _) = x <= y

instance Eq (CommandPacket a) where
  (==) (CommandPacket _ x _) (CommandPacket _ y _) = x == y

-- | Updates of an application's state issued by servers. Are associated with their frame of occurence and the ProcessId of the emitting client.
data UpdatePacket a = UpdatePacket P.ProcessId FrameNr a
  deriving (Generic, Show, Typeable, Eq)
instance Binary a => Binary (UpdatePacket a)
instance HasFrameAssociation (UpdatePacket a) where
  getFrame (UpdatePacket _ n _) = n

-- | Return the data of an update.
updatePacketData :: UpdatePacket a -> a
updatePacketData up = case up of
  UpdatePacket _ _ a -> a

-- | Return the 'P.SendPort' of a 'ServerStateSendPort'
serverStateSendPort :: ServerStateSendPort a -> P.SendPort (UpdatePacket a)
serverStateSendPort sp = case sp of
  ServerStateSendPort p -> p

-- | Return the 'P.ReceivePort' of a 'ServerStateSendPort'
serverStateReceivePort
  :: ServerStateReceivePort a -> P.ReceivePort (UpdatePacket a)
serverStateReceivePort rp = case rp of
  ServerStateReceivePort p -> p

-- | Convenience function that uses 'resolve' to get the 'P.ProcessId' of a 'Node.LocalNode', running in IO
resolveIO :: Resolvable a => Node.LocalNode -> a -> IO (Maybe P.ProcessId)
resolveIO n a = runProcessIO n (resolve a) >>= \mmx -> case mmx of
  Nothing -> return Nothing
  Just mx -> return mx

-- | Searches for a Process under address 'addr' called 'name'. When 'timeLeft' runs out, returns Nothing
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
        Nothing -> do
          P.liftIO $ print "got a reply, but no existing PID"
          searchProcessTimeout name addr (timeLeft - timeoutMS)
      Nothing -> searchProcessTimeout name addr (timeLeft - timeoutMS)
 where
  timeoutMS = 1000
  timeout   = Time.asTimeout $ Time.milliSeconds timeoutMS

-- | Convenience function to create a 'Node.LocalNode'
createLocalNode :: T.Transport -> IO Node.LocalNode
createLocalNode transport = Node.newLocalNode transport Node.initRemoteTable

-- | runs a Process, blocks until it finishes. Result contains a value if the process did not terminate unexpectedly
runProcessIO :: Node.LocalNode -> P.Process a -> IO (Maybe a)
runProcessIO node p = do
  v <- newEmptyTMVarIO
  Node.runProcess node $ do
    result <- p
    P.liftIO $ atomically $ putTMVar v result
  atomically $ tryReadTMVar v
