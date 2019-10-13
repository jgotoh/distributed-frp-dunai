{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}

module Network.Common where

import qualified Control.Distributed.Process   as P
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

-- Channel used by the server to send StateUpdates to clients
-- Maybe rename to ClientStateReceiveChannel
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

newtype JoinRequestResult a = JoinRequestResult (Either JoinError (JoinAccepted a))
  deriving (Generic, Show, Typeable)
instance Binary a => Binary (JoinRequestResult a)

newtype JoinError = JoinError String
  deriving (Generic, Show, Typeable)
instance Binary JoinError

newtype JoinAccepted a = JoinAccepted a
  deriving (Generic, Show, Typeable)
instance Binary a => Binary (JoinAccepted a)

newtype StateUpdate a = StateUpdate a
  deriving (Generic, Show, Typeable)
instance Binary a => Binary (StateUpdate a)

serverStateSendPort :: ServerStateSendPort a -> P.SendPort (StateUpdate a)
serverStateSendPort sp = case sp of ServerStateSendPort p -> p

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

