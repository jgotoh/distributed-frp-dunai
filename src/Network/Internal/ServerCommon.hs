{-# LANGUAGE DeriveGeneric #-}

module Network.Internal.ServerCommon
  ( ServerProcessDefinition
  , Client(..)
  , ServerState
  , joinRequest
  , ServerConfiguration(..)
  , serverProcess
  )
where

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

type ServerProcessDefinition a = MP.ProcessDefinition (ServerState a)

data ServerConfiguration m a = ServerConfiguration
  { hostConfig :: N.HostName
  , portConfig :: N.ServiceName
  ,  nameConfig :: SessionName
  , transportConfig :: m (Either IOException T.Transport)
  , processDefinitionConfig :: ServerProcessDefinition a
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
  :: (Binary a, Typeable a) => ServerProcessDefinition a -> P.Process ()
serverProcess def = MP.serve () initHandler def
  where initHandler _ = return (MP.InitOk [] Time.NoDelay)
