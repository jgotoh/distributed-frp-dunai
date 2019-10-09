{-# LANGUAGE DeriveGeneric #-}

module Network.Common where

import qualified Control.Distributed.Process   as P
import qualified Control.Distributed.Process.Extras.Time
                                               as Time
import qualified Control.Distributed.Process.Node
                                               as Node
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

data JoinRequest = JoinRequest Nickname (P.SendPort (StateUpdate Message))
  deriving (Generic, Show, Typeable)
instance Binary JoinRequest

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

