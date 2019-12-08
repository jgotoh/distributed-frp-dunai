module ClientGame where

import GameState
import Types
import           Data.Maybe
import           FRP.BearRiver.Extra
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

-- TODO:
-- localPlayerSF ist SF, Rest (remotePlayer und ball sind remoteSFs)

-- executed by a host that joins a session
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

-- TODO implement every sf as remote
pureRemoteLoopingGame = undefined

-- TODO this is actually variant 2 of synchronisation (localSFs are simulated)
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

localPlayerSF :: (Monad m) => SF (PlayerEnv m) (GameInput, a) PlayerState
localPlayerSF = arr fst >>> arr directionInput >>> paddleSF

remotePlayerSF
  :: (Monad m)
  => SF (PlayerEnv m) ((Maybe (UpdatePacket NetState)), GameState) PlayerState
remotePlayerSF = arr (uncurry mergePlayerState) >>> arr remotePlayerState

remoteBallSF
  :: Monad m
  => SF (BallEnv m) ((Maybe (UpdatePacket NetState)), GameState) BallState
remoteBallSF = arr (uncurry mergeStates) >>> arr ballState

paddleSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) PlayerState
paddleSF = proc dir -> do
  c      <- colorSF -< undefined
  (p, v) <- moveSF -< dir
  b      <- constM (lift $ asks playerBounds0) -< undefined
  returnA -< PlayerState p b v c

-- if CommandPacket is Nothing, use last GameState, otherwise update it
mergePlayerState :: Maybe (UpdatePacket NetState) -> GameState -> GameState
mergePlayerState mNS gs = case mNS of
  Nothing -> gs
  Just (UpdatePacket _ ns) -> case ns of
    NetState _ rps _ -> gs {remotePlayerState = rps}

colorSF :: Monad m => SF (PlayerEnv m) a Color
colorSF = constM (lift $ asks playerColor0)

-- if CommandPacket is Nothing, use last GameState, otherwise update it
mergeStates :: Maybe (UpdatePacket NetState) -> GameState -> GameState
mergeStates mNS gs = case mNS of
  Nothing                 -> gs
  Just (UpdatePacket _ ns) -> case ns of
    NetState _ _ bs -> gs { ballState = bs }

moveSF :: Monad m => SF (PlayerEnv m) (Maybe Direction) (Position, Velocity)
moveSF = proc dir -> do
  v    <- constM (lift $ asks playerVelocityMax) -< undefined
  p0   <- constM (lift $ asks playerPosition0) -< undefined
  dir' <- arr (fromMaybe (V2 0 0)) -< dir
  v'   <- arr (uncurry (*)) -< (v, dir')
  dp   <- integral -< v'
  p'   <- arr (uncurry (+)) -< (p0, dp)
  returnA -< (p', v')
