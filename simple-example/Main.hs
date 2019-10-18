module Main where

import           Config
import           Display
import           Game
import           GameState
import           Input
import           Network.Client
import           Network.Common
import           Network.Server
import           Time

import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import qualified Control.Distributed.Process   as P
import           Control.Distributed.Process.Extras.Time
import           Control.Monad
import           Control.Monad.STM
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
    ClientConfig ip port nick name server ->
      clientMain ip port nick name server
    ServerConfig ip port name -> launchServer ip (show port) name
    GameConfig                -> gameMain

gameMain :: IO ()
gameMain = do
  (window, renderer) <- initializeSDL
  timeRef            <- createTimeRef

  SDL.showWindow window
  reactimate (return $ GameInput False)
             (sense timeRef)
             (actuate renderer)
             gameSF

  quit window renderer

clientMain :: String -> Int -> String -> String -> String -> IO ()
clientMain ip port nick name serverAddr = do
  (window, renderer) <- initializeSDL
  timeRef            <- createTimeRef

  eNode              <- initializeClientNode ip (show port)

  case eNode of
    Left  ex   -> error $ show ex
    Right node -> do

      mServer <- runProcessResult node (searchForServer name serverAddr)

      case mServer of
        Just (Just server) -> do

          print "Server found"
          (Client pid sQ rQ) <-
            (startClientProcess
              node
              server
              nick
              (createServerStateChannel :: P.Process
                  (ServerStateChannel Message)
              ) :: IO (Client Message)
            )

          SDL.showWindow window

          reactimateNet (return $ GameInput False)
                        (sense timeRef)
                        (actuate renderer)
                        gameSF
                        (receiveState rQ)
                        (createGameInput)
                        (writeState sQ pid)

          quit window renderer
          print "exit client"
        _ -> error "server not found"


receiveState :: TQueue (StateUpdate Message) -> IO (Maybe (StateUpdate Message))
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

writeState :: TQueue (StateUpdate Message) -> P.ProcessId -> GameState -> IO ()
writeState q pid _ = atomically $ writeTQueue q $ StateUpdate pid Pong

createGameInput
  :: ((DTime, GameInput), Maybe (StateUpdate Message)) -> (DTime, GameInput)
createGameInput = fst

initializeSDL :: IO (SDL.Window, SDL.Renderer)
initializeSDL = do
  SDL.initializeAll
  window   <- createWindow "simple-example" windowWidth windowHeight
  renderer <- createRenderer window
  return (window, renderer)

actuate :: SDL.Renderer -> p -> GameState -> IO Bool
actuate renderer _ state = renderGameState renderer state >> return False

sense :: IORef DTime -> Bool -> IO (DTime, Maybe GameInput)
sense timeRef _ = do
  dtSecs <- senseTime timeRef
  events <- SDL.pollEvents
  when (quitEvent events) exitSuccess
  return (dtSecs, Just $ GameInput $ isJump events)
 where
  isJump = Prelude.any (keyPressed SDL.KeycodeSpace)
  quitEvent events = elem SDL.QuitEvent $ map SDL.eventPayload events

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
drawState renderer state = drawCircle renderer $ leftBallPosState state

quit :: SDL.Window -> SDL.Renderer -> IO ()
quit window renderer = do
  putStrLn "quit"
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

