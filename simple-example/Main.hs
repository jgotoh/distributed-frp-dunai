module Main where

import           Config
import           Display
import           Game
import           GameState
import           Input
import           Network.Client
import           Network.Server
import           Time

import qualified Control.Distributed.Process.Node
                                               as Node
import           Data.IORef
import           FRP.BearRiver
import           FRP.BearRiver.Extra
import qualified SDL
import           System.IO

main :: IO ()
main = do
  -- flush output on every newline to support output stream editing via sed
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering

  cfg <- parseConfig

  print cfg

  case cfg of
    ClientConfig ip port nick name server ->
      clientMain ip port nick name server--launchClient ip (show port) nick server name
    ServerConfig ip port name -> launchServer ip (show port) name
    GameConfig                -> main'

main' :: IO ()
main' = do
  (window, renderer) <- initializeSDL
  timeRef            <- createTimeRef

  SDL.showWindow window
  reactimate (return $ GameInput False)
             (sense timeRef)
             (actuate renderer)
             gameSF

  quit window renderer

clientMain :: String -> Int -> String -> String -> String -> IO ()
clientMain ip port nick name server = do
  (window, renderer) <- initializeSDL
  timeRef            <- createTimeRef

  eNode              <- initializeClientNode ip (show port)

  case eNode of
    Left  ex   -> error $ show ex
    Right node -> do

      _ <- startClientNetworkProcess node


      SDL.showWindow window
      reactimateNet (return $ GameInput False)
                    (sense timeRef)
                    (actuate renderer)
                    gameSF
                    (sendState node)

      quit window renderer


sendState :: Node.LocalNode -> GameState -> IO ()
sendState node x = do
  _ <- Node.forkProcess node $ do
    return ()
  return ()


initializeSDL :: IO (SDL.Window, SDL.Renderer)
initializeSDL = do
  SDL.initializeAll
  window   <- createWindow "simple-example" windowWidth windowHeight
  renderer <- createRenderer window
  return (window, renderer)

-- TODO send message
actuate :: SDL.Renderer -> p -> GameState -> IO Bool
actuate renderer _ state = renderGameState renderer state >> return False --qPressed

-- TODO receive message
sense :: IORef DTime -> Bool -> IO (DTime, Maybe GameInput)
sense timeRef _ = do
  dtSecs <- senseTime timeRef
  events <- SDL.pollEvents
  return (dtSecs, Just $ GameInput $ isJump events)
  where isJump = Prelude.any (keyPressed SDL.KeycodeSpace)

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

quit :: SDL.Window -> SDL.Renderer -> IO ()
quit window renderer = do
  putStrLn "quit"
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

