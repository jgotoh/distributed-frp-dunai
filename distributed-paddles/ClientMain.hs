module ClientMain where

import           Config
import           Display
import           GameState
import           ClientGame
import           ServerMain
import           ProcessExtra
import           Network.Client
import           Network.Common
import           Time
import           Types

import           Control.Applicative
import           Control.Concurrent.STM.TMVar
import qualified Control.Distributed.Process   as P

import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.Trans.MSF.Reader
import           Data.IORef
import           FRP.BearRiver
import           FRP.BearRiver.Network.Reactimate
import qualified SDL
import           System.Exit

clientMain
  :: HostName -> Port -> Nickname -> SessionName -> ServerAddress -> Bool -> DRMConfig -> IO ()
clientMain ip port nick session addr csp drm = do
  (window, renderer) <- initializeSDL "Distributed Paddles"
  Right (node, _)    <- initializeNode ip port

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

  -- FPS:
  frameNrRef <- newIORef 0
  startTime <- createTimeRef

  timeRef   <- createTimeRef
  reactimateClient (return $ GameInput Nothing)
                   (sense timeRef)
                   (actuate frameNrRef renderer)
                   (runGameReader gs (dynamicRemoteClientSF csp drm))
                   (receiveState rQ)
                   (writeCommand' getDir sQ pid)

  dtTime <- senseTime startTime
  frames <- readIORef frameNrRef

  let dtMs = dtTime
      fps = (fromIntegral frames ) / dtMs

  print $ "FPS: " ++ show fps

  quit window renderer

clientWindowTitle :: PlayerSettings -> PlayerSettings -> String
clientWindowTitle localP remoteP = if x localP < x remoteP
  then "Distributed Paddles: Left"
  else "Distributed Paddles: Right"
 where
  x ps = case playerPosition0 ps of
    SDL.V2 x' _ -> x'

getDir :: (DTime, GameInput) -> Maybe Command
getDir = mkCommand . directionInput . snd where mkCommand = (Command <$>)

writeCommand'
  :: ((DTime, GameInput) -> Maybe cmd)
  -> TMVar (CommandPacket cmd)
  -> P.ProcessId
  -> FrameNr
  -> (DTime, GameInput)
  -> IO ()
writeCommand' f q pid frame x = case f x of
  Nothing -> return ()
  Just c  -> writeCommand q $ CommandPacket pid frame c

runGameReader :: Monad m => GameSettings -> SF (GameEnv m) a b -> SF m a b
runGameReader gs sf = readerS $ runReaderS_ (runReaderS sf) gs

actuate :: IORef Integer -> SDL.Renderer -> p -> GameState -> IO Bool
actuate frameNrRef renderer _ state = do

  previousFrame <- readIORef frameNrRef
  writeIORef frameNrRef $ previousFrame + 1

  renderGameState renderer state
  return (gameOver state)

sense :: IORef DTime -> Bool -> IO (DTime, Maybe GameInput)
sense timeRef _ = do
  _ <- fixedTimeStep 33.333 timeRef
  events <- SDL.pollEvents
  when (quitEvent events) exitSuccess
  dir <- direction
  -- print dtSecs
  return (0.033333, Just $ GameInput dir)
  -- return (0.0333, Just $ GameInput dir)
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
drawBall r bs = drawCircle r (ballPositionState bs) (ballRadiusState bs) (ballColorState bs)

