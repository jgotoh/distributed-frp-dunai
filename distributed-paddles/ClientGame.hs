{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module ClientGame
  ( remoteClientSF
  , dynamicRemoteClientSF
  )
where

import Types
import           Config
import           ServerGame
import           FRP.BearRiver.DeadReckoning
import           Data.MonadicStreamFunction.DeadReckoning
import           Data.MonadicStreamFunction.Extra
import           Control.Monad.Trans.MSF.Reader
import           Network.Common
import           GameState
import           FRP.BearRiver           hiding ( dot
                                                , (^+^)
                                                )
import           SDL.Vect                hiding ( identity
                                                , trace
                                                )
import           Control.Monad.Reader           ( lift )

-- Executed by a client that joins a session. Only displays states received via UpdatePackets. Uses Client Side Prediction and first-order Dead Reckoning.
remoteClientSF
  :: Monad m
  => SF (GameEnv m) (GameInput, (Maybe (UpdatePacket NetState))) GameState
remoteClientSF = feedbackM act pureRemoteLoopingGame
 where
  act = do
    gs <- lift ask
    return (toState gs)
  toState gs = GameState (ps0 gs) (rps0 gs) (bs0 gs) False
  ps0  = toPlayerState . localPlayerSettings
  bs0  = toBallState . ballSettings
  rps0 = toPlayerState . remotePlayerSettings

-- Executed by a client that joins a session. Only displays states received via UpdatePackets. Uses Client Side Prediction and first-order Dead Reckoning.
-- This function allows to switch between consistency maintenance mechanisms.
dynamicRemoteClientSF
  :: Monad m
  => Bool -- whether to use client side prediction
  -> DRMConfig
  -> SF (GameEnv m) (GameInput, (Maybe (UpdatePacket NetState))) GameState
dynamicRemoteClientSF csp drm' = feedbackM act $ getRemoteLoopingSF csp drm'
 where
  act = do
    gs <- lift ask
    return (toState gs)
  toState gs = GameState (ps0 gs) (rps0 gs) (bs0 gs) False
  ps0  = toPlayerState . localPlayerSettings
  bs0  = toBallState . ballSettings
  rps0 = toPlayerState . remotePlayerSettings

-- Every game object is simulated by the server
pureRemoteLoopingGame
  :: Monad m
  => SF
       (GameEnv m)
       ((GameInput, (Maybe (UpdatePacket NetState))), GameState)
       (GameState, GameState)
pureRemoteLoopingGame =
  (arr fst >>> morphS (selectEnv localPlayerSettings) localRemotePlayerSFCSP)
    &&& (arr su >>> morphS (selectEnv remotePlayerSettings) remotePlayerSF)
    &&& (arr su >>> morphS (selectEnv ballSettings) remoteBallSF)
    &&& (arr shouldQuit)
    >>> arr (\(ps, (ps', (bs, q))) -> (((ps, ps'), bs), q))
    >>> arr ((uncurry . uncurry . uncurry) GameState)
    >>> arr dup
  where su = snd . fst

-- This function is a bit complicated due to dynamic selection of consistency maintenance mechanisms.
-- See 'remoteLoopingGame' for a better version that uses Client Side Prediction and first-order Dead Reckoning.
getRemoteLoopingSF
  :: Monad m
  => Bool -- whether to use client side prediction
  -> DRMConfig
  -> SF
       (GameEnv m)
       ((GameInput, (Maybe (UpdatePacket NetState))), GameState)
       (GameState, GameState)
getRemoteLoopingSF csp drm' =
    getPlayerSF
    &&& getRemotePlayerSF
    &&& getRemoteBallSF
    &&& (arr shouldQuit)
    >>> arr (\(ps, (ps', (bs, q))) -> (((ps, ps'), bs), q))
    >>> arr ((uncurry . uncurry . uncurry) GameState)
    >>> arr dup
  where
    su = snd . fst
    getPlayerSF = (arr fst >>> morphS (selectEnv localPlayerSettings) (if csp then localRemotePlayerSFCSP else localRemotePlayerSF))
    getRemotePlayerSF = (arr su >>> morphS (selectEnv remotePlayerSettings) decideRemotePlayer)
    decideRemotePlayer = case drm' of
      DRMZero -> remotePlayerSF0
      DRMFirst -> remotePlayerSF
    getRemoteBallSF = (arr su >>> morphS (selectEnv ballSettings) decideRemoteBall)
    decideRemoteBall = case drm' of
      DRMZero -> remoteBallSF0
      DRMFirst -> remoteBallSF

-- returns true, if NetState.gameOverNetState returns true
shouldQuit :: ((GameInput, Maybe (UpdatePacket NetState)), GameState) -> Bool
shouldQuit tuple = maybe False id (isQuit <$> (snd $ fst tuple))
  where
    isQuit = gameOverNetState . updatePacketData

selectEnv
  :: (GameSettings -> a) -> ClockInfo (ReaderT a m) c -> ClockInfo (GameEnv m) c
selectEnv f = mapReaderT $ withReaderT f

-- local player: zero-order DRM
localRemotePlayerSF
  :: Monad m => SF (PlayerEnv m) (GameInput, Maybe (UpdatePacket NetState)) PlayerState
localRemotePlayerSF =
  arr snd >>> arr (fmap (localPlayerNetState . updatePacketData)) >>> drmZero state0
 where
  state0 = PlayerState zeroVector zeroVector zeroVector white

-- local player: Client Side Prediction
localRemotePlayerSFCSP :: Monad m => SF (PlayerEnv m) (GameInput, Maybe (UpdatePacket NetState)) PlayerState
localRemotePlayerSFCSP =  arr directionInput *** arr (fmap (localPlayerNetState . updatePacketData))
  >>> predict paddleSF (\ps -> morphS (mapReaderT (local $ newPos ps) ) paddleSF )
  where
    newPos ps pset = pset{playerPosition0 = playerPositionState ps, playerVelocityMax = (playerVelocityMax pset) SDL.Vect.^/ 2}

-- remote player: first-order DRM
remotePlayerSF
  :: (Monad m) => SF (PlayerEnv m) ((Maybe (UpdatePacket NetState))) PlayerState
remotePlayerSF =
  arr (fmap (remotePlayerNetState . updatePacketData)) >>> drmFirst state0 new
 where
  new ps pos = ps { playerPositionState = pos }
  state0 = PlayerState zeroVector zeroVector zeroVector white

-- remote player: zero-order DRM
remotePlayerSF0
  :: (Monad m) => SF (PlayerEnv m) ((Maybe (UpdatePacket NetState))) PlayerState
remotePlayerSF0 =
  arr (fmap (remotePlayerNetState . updatePacketData)) >>> drmZero state0
 where
  state0 = PlayerState zeroVector zeroVector zeroVector white

-- ball: first-order DRM
remoteBallSF
  :: Monad m => SF (BallEnv m) ((Maybe (UpdatePacket NetState))) BallState
remoteBallSF =
  arr (fmap (ballNetState . updatePacketData)) >>> drmFirst state0 new
 where
  new ps pos = ps { ballPositionState = pos }
  state0 = BallState zeroVector 50 zeroVector white

-- ball: zero-order DRM
remoteBallSF0
  :: Monad m => SF (BallEnv m) ((Maybe (UpdatePacket NetState))) BallState
remoteBallSF0 =
  arr (fmap (ballNetState . updatePacketData)) >>> drmZero state0
 where
  state0 = BallState zeroVector 50 zeroVector white

white :: Color
white = V4 255 255 255 255
