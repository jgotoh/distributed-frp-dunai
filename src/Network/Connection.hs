{-# LANGUAGE OverloadedStrings #-}

module Network.Connection where

import Control.Monad (forever)
import Network.Socket
import Network.Socket.ByteString

createSession :: HostName -> ServiceName -> IO ()
createSession ip port =
  withSocketsDo $ do
    print ("Creating a session" :: String )

    -- create local UDP socket
    addrA <- resolveAddress (Just ip) port Datagram
    sockA <- createSocket addrA
    bind sockA (addrAddress addrA)

    print $ show addrA
    print ("Waiting for remote to connect" :: String )

    _ <- forever $ do
      (msg, remote) <- recvFrom sockA 1024
      _ <- sendTo sockA "hello from master" remote
      print msg
      print remote
    return ()

joinSession :: HostName -> ServiceName -> IO ()
joinSession ip port =
  withSocketsDo $ do
    print ("Joining a session" :: String )

    -- create remote host UDP socket
    addrA <- resolveAddress (Just ip) port Datagram
    sockA <- createSocket addrA

    print $ show addrA
    print ("Sending message to remote host" :: String )

    _ <- forever $ do
      _ <- sendTo sockA "hello from remote" $ addrAddress addrA
      (msg, remote) <- recvFrom sockA 1024
      print msg
      print remote
    return ()

resolveAddress :: Maybe HostName -> ServiceName -> SocketType -> IO AddrInfo
resolveAddress ip port socketType = do
    let hints = defaultHints {
           addrFamily = AF_INET,
           addrSocketType = socketType
          }

    addr:_ <- getAddrInfo (Just hints) ip (Just port)
    return addr

createSocket :: AddrInfo -> IO Socket
createSocket addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    withFdSocket sock setCloseOnExecIfNeeded -- automatically close after execution
    return sock

