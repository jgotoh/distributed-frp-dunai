module ClientMain where

import           Display
import           GameState
import           ClientGame
import           ServerMain
import           Network.Client
import           Network.Common
import           Time
import           Types

import           Control.Applicative
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TMVar
import qualified Control.Distributed.Process   as P

import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.Trans.MSF.Reader
import           Data.IORef
import           FRP.BearRiver
import           FRP.BearRiver.Extra
import qualified SDL
import           System.Exit

clientMain
  :: HostName -> Port -> Nickname -> SessionName -> ServerAddress -> IO ()
clientMain ip port nick session addr = do
  (window, renderer) <- initializeSDL "distributed-paddles"
  timeRef            <- createTimeRef
  Right (node, _)    <- initializeNode ip port

  -- test TCP connection
  -- searchForServerEndPoint transport session addr

  Just  server       <- runProcessIO node (searchForServer session addr)
    >>= \s -> return $ join s

  print "Found Server"

  ((LocalClient pid rQ sQ), joinResult) <-
    (startClientProcess
      node
      server
      nick
      (createServerStateChannel :: P.Process (ServerStateChannel NetState))
    )

  SDL.showWindow window

  JoinRequestResult (Right (JoinAccepted _)) <- atomically
    $ takeTMVar joinResult

  -- get initial GameSettings at time = 0
  Just gs <- runProcessIO node (reqGameSettings server pid)
    >>= \s -> return $ join s

  setWindowTitle
    window
    (clientWindowTitle (localPlayerSettings gs) (remotePlayerSettings gs))

  reactimateClient (return $ GameInput Nothing)
                   (sense timeRef)
                   (actuate renderer)
                   (runGameReader gs remoteClientSF)
                   (receiveState rQ)
                   (writeState getDir sQ pid)

  quit window renderer

clientWindowTitle :: PlayerSettings -> PlayerSettings -> String
clientWindowTitle localP remoteP = if x localP < x remoteP
  then "distributed-paddles: Left"
  else "distributed-paddles: Right"
 where
  x ps = case playerPosition0 ps of
    SDL.V2 x' _ -> x'

receiveState
  :: TQueue (UpdatePacket NetState) -> IO (Maybe (UpdatePacket NetState))
receiveState q = do
  in' <- readQ q
  -- print in'
  return in'
  where readQ = atomically . tryReadTQueue

getDir :: (DTime, GameInput) -> Maybe Command
getDir = mkCommand . directionInput . snd where mkCommand = (Command <$>)

-- TODO as library function, so user only has to write f
writeState
  :: ((DTime, GameInput) -> Maybe Command)
  -> TQueue (CommandPacket Command)
  -> P.ProcessId
  -> FrameNr
  -> (DTime, GameInput)
  -> IO ()
writeState f q pid frame x = case f x of
  Nothing -> return ()
  Just c  -> atomically . writeTQueue q $ CommandPacket pid frame c

runGameReader :: Monad m => GameSettings -> SF (GameEnv m) a b -> SF m a b
runGameReader gs sf = readerS $ runReaderS_ (runReaderS sf) gs

actuate :: SDL.Renderer -> p -> GameState -> IO Bool
actuate renderer _ state = do
  renderGameState renderer state >> return False

sense :: IORef DTime -> Bool -> IO (DTime, Maybe GameInput)
sense timeRef _ = do
  dtSecs <- fixedTimeStep 16.6 timeRef
  events <- SDL.pollEvents
  when (quitEvent events) exitSuccess
  dir <- direction
  return (dtSecs, Just $ GameInput dir)
  where quitEvent events = elem SDL.QuitEvent $ map SDL.eventPayload events

direction :: IO (Maybe Direction)
direction = do
  isKey <- SDL.getKeyboardState
  return $ boolToMaybe isKey SDL.ScancodeUp (SDL.V2 0 1) <|> boolToMaybe
    isKey
    SDL.ScancodeDown
    (SDL.V2 0 $ -1)

boolToMaybe
  :: (SDL.Scancode -> Bool) -> SDL.Scancode -> Direction -> Maybe Direction
boolToMaybe f code v = if f code then Just v else Nothing

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event' = case SDL.eventPayload event' of
  SDL.KeyboardEvent keyboardEvent ->
    SDL.keyboardEventKeyMotion keyboardEvent
      == SDL.Pressed
      && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
      == SDL.KeycodeQ
  _ -> False

qPressed :: IO Bool
qPressed = Prelude.any eventIsQPress <$> SDL.pollEvents

renderGameState :: SDL.Renderer -> GameState -> IO ()
renderGameState renderer state = do
  drawBackground renderer
  drawState renderer state
  SDL.present renderer

drawState :: SDL.Renderer -> GameState -> IO ()
drawState renderer state = do
  drawPlayer renderer $ firstPlayer state
  drawPlayer renderer $ secondPlayer state
  drawBall renderer $ ballState state
 where
  firstPlayer  = localPlayerState
  secondPlayer = remotePlayerState

drawPlayer :: SDL.Renderer -> PlayerState -> IO ()
drawPlayer r ps = drawRect r
                           (playerPositionState ps)
                           (playerBoundsState ps)
                           (playerColorState ps)

drawBall :: SDL.Renderer -> BallState -> IO ()
drawBall r bs = drawCircle r (ballPositionState bs) (ballRadiusState bs)

