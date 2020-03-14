{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module ClientGame
  ( remoteClientSF
  )
where

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

-- executed by a host that joins a session. Only displays states received via UpdatePackets
remoteClientSF
  :: Monad m
  => SF (GameEnv m) (GameInput, (Maybe (UpdatePacket NetState))) GameState
remoteClientSF = feedbackM act pureRemoteLoopingGame
 where
  act = do
    gs <- lift ask
    return (toState gs)
  toState gs = GameState (ps0 gs) (rps0 gs) (bs0 gs)
  ps0  = toPlayerState . localPlayerSettings
  bs0  = toBallState . ballSettings
  rps0 = toPlayerState . remotePlayerSettings

-- Every game object is simulated by the server
pureRemoteLoopingGame
  :: (Monad m)
  => SF
       (GameEnv m)
       ((GameInput, (Maybe (UpdatePacket NetState))), GameState)
       (GameState, GameState)
pureRemoteLoopingGame =
  -- (arr su >>> morphS (selectEnv localPlayerSettings) localRemotePlayerSF')
  (arr fst >>> morphS (selectEnv localPlayerSettings) localRemotePlayerSF)
    &&& (arr su >>> morphS (selectEnv remotePlayerSettings) remotePlayerSF)
    &&& (arr su >>> morphS (selectEnv ballSettings) remoteBallSF)
    >>> arr (\(ps, (ps', bs)) -> ((ps, ps'), bs))
    >>> arr ((uncurry . uncurry) GameState)
    >>> arr dup
  where su = snd . fst

selectEnv
  :: (GameSettings -> a) -> ClockInfo (ReaderT a m) c -> ClockInfo (GameEnv m) c
selectEnv f = mapReaderT $ withReaderT f

-- using DRM
localRemotePlayerSF'
  :: Monad m => SF (PlayerEnv m) (Maybe (UpdatePacket NetState)) PlayerState
localRemotePlayerSF' =
  arr (fmap (localPlayerNetState . updatePacketData)) >>>
  -- drmZero state0
                                                          drmFirst state0 new
 where
  new ps pos = ps { playerPositionState = pos }
  state0 = PlayerState zeroVector zeroVector zeroVector (V4 255 255 255 255)

-- using LocalPrediction
localRemotePlayerSF :: Monad m => SF (PlayerEnv m) (GameInput, Maybe (UpdatePacket NetState)) PlayerState
localRemotePlayerSF =  arr directionInput *** arr (fmap (localPlayerNetState . updatePacketData))
  >>> predict paddleSF (\ps -> morphS (mapReaderT (local $ newPos ps) ) paddleSF )
  where
    newPos ps pset = pset{playerPosition0 = playerPositionState ps, playerVelocityMax = (playerVelocityMax pset) SDL.Vect.^/ 2}

remotePlayerSF
  :: (Monad m) => SF (PlayerEnv m) ((Maybe (UpdatePacket NetState))) PlayerState
remotePlayerSF =
  arr (fmap (remotePlayerNetState . updatePacketData)) >>>
  -- drmZero state0
                                                           drmFirst state0 new
 where
  new ps pos = ps { playerPositionState = pos }
  state0 = PlayerState zeroVector zeroVector zeroVector (V4 255 255 255 255)

remoteBallSF
  :: Monad m => SF (BallEnv m) ((Maybe (UpdatePacket NetState))) BallState
remoteBallSF =
  arr (fmap (ballNetState . updatePacketData)) >>>
  -- drmZero state0
                                                   drmFirst state0 new
 where
  new ps pos = ps { ballPositionState = pos }
  state0 = BallState zeroVector 50 zeroVector (V4 255 255 255 255)

