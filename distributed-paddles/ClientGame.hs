module ClientGame where

import           Types
import           Data.Maybe
import           Control.Applicative
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

-- executed by a host that joins a session. Simulates its local paddle
clientSF
  :: Monad m
  => SF (GameEnv m) (GameInput, (Maybe (UpdatePacket NetState))) GameState
clientSF = feedbackM act remoteLoopingGame
 where
  act = do
    gs <- lift ask
    return (toState gs)
  toState gs = GameState (ps0 gs) (rps0 gs) (bs0 gs)
  ps0  = toPlayerState . localPlayerSettings
  bs0  = toBallState . ballSettings
  rps0 = toPlayerState . remotePlayerSettings

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
  (arr su_gs >>> morphS (selectEnv localPlayerSettings) localRemotePlayerSF)
    &&& (arr su_gs >>> morphS (selectEnv remotePlayerSettings) remotePlayerSF)
    &&& (arr su_gs >>> morphS (selectEnv ballSettings) remoteBallSF)
    >>> arr (\(ps, (ps', bs)) -> ((ps, ps'), bs))
    >>> arr ((uncurry . uncurry) GameState)
    >>> arr dup
 where
  su_gs ((_, s), gs) = (s, gs)

-- Locally controlled paddle is simulated, rest is simulated by the server
remoteLoopingGame
  :: (Monad m)
  => SF
       (GameEnv m)
       ((GameInput, (Maybe (UpdatePacket NetState))), GameState)
       (GameState, GameState)
remoteLoopingGame =
  (arr gi_gs >>> morphS (selectEnv localPlayerSettings) localPlayerSF)
    &&& (arr su_gs >>> morphS (selectEnv remotePlayerSettings) remotePlayerSF)
    &&& (arr su_gs >>> morphS (selectEnv ballSettings) remoteBallSF)
    >>> arr (\(ps, (ps', bs)) -> ((ps, ps'), bs))
    >>> arr ((uncurry . uncurry) GameState)
    >>> arr dup
 where
  gi_gs ((gi, _), gs) = (gi, gs)
  su_gs ((_, s), gs) = (s, gs)

selectEnv
  :: (GameSettings -> a) -> ClockInfo (ReaderT a m) c -> ClockInfo (GameEnv m) c
selectEnv f = mapReaderT $ withReaderT f

-- returns localPlayerNetState or last one from GameState
localRemotePlayerSF
  :: (Monad m)
  => SF (PlayerEnv m) ((Maybe (UpdatePacket NetState)), GameState) PlayerState
localRemotePlayerSF = arr (fromNetState localPlayerNetState localPlayerState) >>> arr fromJust

-- returns remotePlayerNetState or last one from GameState
remotePlayerSF
  :: (Monad m)
  => SF (PlayerEnv m) ((Maybe (UpdatePacket NetState)), GameState) PlayerState
remotePlayerSF = arr (fromNetState remotePlayerNetState remotePlayerState) >>> arr fromJust

-- returns ballNetState or last one from GameState
remoteBallSF
  :: Monad m
  => SF (BallEnv m) ((Maybe (UpdatePacket NetState)), GameState) BallState
remoteBallSF = arr (fromNetState ballNetState ballState) >>> arr fromJust

-- Return state of a from an UpdatePacket t1 if available, or return last state of GameState t2
fromNetState :: (t1 -> a)
                      -> (t2 -> a) -> (Maybe (UpdatePacket t1), t2) -> Maybe a
fromNetState fNS fGS (up, gs) = get <$> up <|> Just (fGS gs)
  where
    get x = case x of
      UpdatePacket _ ns -> fNS ns

-- Signal Functions to simulate a locally controlled paddle
localPlayerSF :: (Monad m) => SF (PlayerEnv m) (GameInput, a) PlayerState
localPlayerSF = arr fst >>> arr directionInput >>> paddleSF

paddleSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) PlayerState
paddleSF = proc dir -> do
  c      <- colorSF -< undefined
  (p, v) <- moveSF -< dir
  b      <- constM (lift $ asks playerBounds0) -< undefined
  returnA -< PlayerState p b v c

colorSF :: Monad m => SF (PlayerEnv m) a Color
colorSF = constM (lift $ asks playerColor0)

moveSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) (Position, Velocity)
moveSF = proc dir -> do
  v    <- constM (lift $ asks playerVelocityMax) -< undefined
  p0   <- constM (lift $ asks playerPosition0) -< undefined
  dir' <- arr (fromMaybe (V2 0 0)) -< dir
  v'   <- arr (uncurry (*)) -< (v, dir')
  dp   <- integral -< v'
  p'   <- arr (uncurry (+)) -< (p0, dp)
  returnA -< (p', v')
