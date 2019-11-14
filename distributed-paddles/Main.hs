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
--import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TMVar
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
    ClientConfig ip port nick name server ->
      clientMain ip port nick name server
    ServerConfig ip port name -> startServerProcess
      ip
      (show port)
      name
      (serverProcessDef :: ServerProcessDefinition NetState)
    GameConfig -> undefined

clientMain :: Show a => String -> a -> String -> String -> String -> IO ()
clientMain ip port nick name serverAddr = do
  (window, renderer) <- initializeSDL "distributed-paddles"
  timeRef            <- createTimeRef
  eNode              <- initializeClientNode ip (show port)

  case eNode of
    Left  ex   -> error $ show ex
    Right node -> do

      mServer <- runProcessResult node (searchForServer name serverAddr)

      case mServer of
        Nothing            -> error "Server could not be found"
        Just (Nothing    ) -> error "Server could not be found"
        Just (Just server) -> do

          print "Found Server"

          ((Client pid sQ rQ), joinResult) <-
            (startClientProcess
              node
              server
              nick
              (createServerStateChannel :: P.Process
                  (ServerStateChannel NetState)
              )
            )

          SDL.showWindow window

          res <- atomically $ takeTMVar joinResult

          case res of
            JoinRequestResult r -> case r of
              Left  err               -> error $ show err
              Right (JoinAccepted cs) -> do

                if null cs
                  then reactimateNet' (return $ GameInput Nothing)
                                      (sense timeRef)
                                      (actuate renderer)
                                      (runGameReader firstGS gameSF)
                                      (receiveState rQ)
                                      (writeState (hostANetState) sQ pid)
                  else reactimateNet' (return $ GameInput Nothing)
                                      (sense timeRef)
                                      (actuate renderer)
                                      (runGameReader secondGS remoteGameSF)
                                      (receiveState rQ)
                                      (writeState (hostBNetState) sQ pid)

          quit window renderer
 where
  firstPlayer = PlayerSettings (SDL.V2 50 100)
                               (SDL.V2 10 50)
                               (SDL.V2 0 175)
                               firstPlayerColor
  ball = BallSettings (SDL.V2 200 150) 4 (SDL.V2 350 350) firstPlayerColor
  firstGS          = GameSettings firstPlayer secondPlayer ball
  firstPlayerColor = SDL.V4 240 142 125 255
  secondPlayer     = PlayerSettings (SDL.V2 300 100)
                                    (SDL.V2 10 50)
                                    (SDL.V2 0 175)
                                    firstPlayerColor
  secondGS = GameSettings secondPlayer firstPlayer ball

hostANetState :: GameState -> NetState
hostANetState gs = NetState dir ball
 where
  dir  = SDL.normalize (playerVelocityState . localPlayerState $ gs)
  ball = Just $ ballState gs

hostBNetState :: GameState -> NetState
hostBNetState gs = NetState dir Nothing
  where dir = SDL.normalize (playerVelocityState . localPlayerState $ gs)

receiveState
  :: TQueue (StateUpdate NetState) -> IO (Maybe (StateUpdate NetState))
receiveState q = readQ q where readQ = atomically . tryReadTQueue

writeState
  :: (GameState -> NetState)
  -> TQueue (StateUpdate NetState)
  -> P.ProcessId
  -> GameState
  -> IO ()
writeState f q pid gs = do
  -- TODO replace with sending network output states at a fixed rate (see FRP2016 paper)
  threadDelay 10000
  atomically $ writeTQueue q $ StateUpdate pid $ f gs

runGameReader :: Monad m => GameSettings -> SF (GameEnv m) a b -> SF m a b
runGameReader gs sf = readerS $ runReaderS_ (runReaderS sf) gs

actuate :: SDL.Renderer -> p -> GameState -> IO Bool
actuate renderer _ state = renderGameState renderer state >> return False

sense :: IORef DTime -> Bool -> IO (DTime, Maybe GameInput)
sense timeRef _ = do
  dtSecs <- senseTime timeRef
  events <- SDL.pollEvents
  when (quitEvent events) exitSuccess
  dir <- direction
  --print $ "dt: " ++ show dtSecs
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
  drawCircle renderer (SDL.V2 0 0) 30
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

