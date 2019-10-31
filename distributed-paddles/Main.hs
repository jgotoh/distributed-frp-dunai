module Main where

import           Config
import           Display
import           Game
import           GameState
--import           Input
import           Network.Client
import           Network.Common
import           Network.Server
import           Time
import           Types

import           Control.Applicative
import           Control.Exception
--import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import qualified Control.Distributed.Process   as P
--import           Control.Distributed.Process.Extras.Time
import           Control.Monad
--import Control.Monad.Reader (lift)
import           Control.Monad.STM
import           Control.Monad.Trans.MSF.Reader
import           Data.IORef
import           FRP.BearRiver
import           FRP.BearRiver.Extra
import qualified SDL
import           System.IO
import           System.Exit

main :: IO ()
main = do
  -- flush output on every newline to support output stream editing via sed
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering

  cfg <- parseConfig

  print cfg

  case cfg of
    ClientConfig ip port nick name server -> clientMain ip port nick name server
    ServerConfig ip port name     -> startServerProcess ip (show port) name (serverProcessDef :: ServerProcessDefinition GameState)
    GameConfig             -> gameMain

gameMain :: IO ()
gameMain = do
  (window, renderer) <- initializeSDL "distributed-paddles"
  timeRef            <- createTimeRef

  SDL.showWindow window

  reactimate (return $ GameInput Nothing)
             (sense timeRef)
             (actuate renderer)
             (runGameReader gs gameSF)

  quit window renderer
 where
  localPlayer = PlayerSettings (SDL.V2 50 100)
                               (SDL.V2 10 50)
                               (SDL.V2 0 175)
                               localPlayerColor
  ball = BallSettings (SDL.V2 200 150) 4 (SDL.V2 350 350) localPlayerColor
  gs = GameSettings localPlayer ball
  localPlayerColor = SDL.V4 240 142 125 255

clientMain ip port nick name serverAddr = do
  (window, renderer) <- initializeSDL "distributed-paddles"
  timeRef            <- createTimeRef

  eNode <- initializeClientNode ip (show port)

  case eNode of
    Left ex -> error $ show ex
    Right node -> do

      mServer <- runProcessResult node (searchForServer name serverAddr)

      case mServer of
        Just (Just server) -> do

          print "Found Server"

          (Client pid sQ rQ) <-
            (startClientProcess
              node
              server
              nick
              (createServerStateChannel :: P.Process
                  (ServerStateChannel GameState)
              ) :: IO (Client GameState)
            )

          SDL.showWindow window

          -- TODO differentiate between host and join session, depending on joinResult
          reactimateNet (return $ GameInput Nothing)
                        (sense timeRef)
                        (actuate renderer)
                        (runGameReader gs gameSF)
                        (receiveState rQ)
                        fst
                        (writeState sQ pid)

          quit window renderer
 where
  localPlayer = PlayerSettings (SDL.V2 50 100)
                               (SDL.V2 10 50)
                               (SDL.V2 0 175)
                               localPlayerColor
  ball = BallSettings (SDL.V2 200 150) 4 (SDL.V2 350 350) localPlayerColor
  gs = GameSettings localPlayer ball
  localPlayerColor = SDL.V4 240 142 125 255

receiveState :: TQueue (StateUpdate GameState) -> IO (Maybe (StateUpdate GameState))
receiveState q =
  readQ q
    >>= (\m -> do
          case m of
            Nothing -> return m
            Just x  -> do
              print $ "rec:" ++ show x
              return m
        )
  where readQ = atomically . tryReadTQueue

writeState :: TQueue (StateUpdate GameState) -> P.ProcessId -> GameState -> IO ()
writeState q pid gs = atomically $ writeTQueue q $ StateUpdate pid gs

runGameReader
  :: Monad m
  => GameSettings
  -> SF (GameEnv m) GameInput GameState
  -> SF m GameInput GameState
runGameReader gs sf = readerS $ runReaderS_ (runReaderS sf) gs

actuate :: SDL.Renderer -> p -> GameState -> IO Bool
actuate renderer _ state = renderGameState renderer state >> return False

sense :: IORef DTime -> Bool -> IO (DTime, Maybe GameInput)
sense timeRef _ = do
  dtSecs <- senseTime timeRef
  events <- SDL.pollEvents
  when (quitEvent events) exitSuccess
  dir <- direction
  return (dtSecs, Just $ GameInput dir)
  where quitEvent events = elem SDL.QuitEvent $ map SDL.eventPayload events

direction :: IO (Maybe Direction)
direction = do
  isKey <- SDL.getKeyboardState
  return
    $   boolToMaybe isKey SDL.ScancodeUp (SDL.V2 0 1)
    <|> boolToMaybe isKey SDL.ScancodeDown (SDL.V2 0 $ -1)

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
  drawPlayer renderer $ localPlayer state
  drawBall renderer $ ballState state
  drawCircle renderer (SDL.V2 0 0) 30
  where localPlayer = localPlayerState

drawPlayer :: SDL.Renderer -> PlayerSettings -> IO ()
drawPlayer r ps =
  drawRect r (playerPosition ps) (playerBounds ps) (playerColor ps)

drawBall :: SDL.Renderer -> BallSettings -> IO ()
drawBall r bs = drawCircle r (ballPosition bs) (ballRadius bs)

