module Main where

import Config
import Display
import Game
import GameState
import Input
import Network.Connection
import Time

import Data.IORef
import FRP.BearRiver
import qualified SDL

main :: IO ()
main = do
  cfg <- parseConfig

  print cfg

  if createSessionConfig cfg then createSession (ipConfig cfg) (show $ portConfig cfg) else joinSession (ipConfig cfg) (show $ portConfig cfg)

main' :: IO ()
main' = do
  (window, renderer) <- initializeSDL
  timeRef <- createTimeRef

  SDL.showWindow window
  reactimate (return $ GameInput False) (sense timeRef) (actuate renderer) gameSF

  quit window renderer

initializeSDL :: IO (SDL.Window, SDL.Renderer)
initializeSDL = do
  SDL.initializeAll
  window <- createWindow "simple-example" windowWidth windowHeight
  renderer <- createRenderer window
  return (window, renderer)

actuate :: SDL.Renderer -> p -> GameState -> IO Bool
actuate renderer _ state = renderGameState renderer state >> return False --qPressed

sense ::IORef DTime -> Bool -> IO (DTime, Maybe GameInput)
sense timeRef _ = do
  dtSecs <- senseTime timeRef
  events <- SDL.pollEvents
  return (dtSecs, Just $ GameInput $ isJump events)
  where
    isJump = Prelude.any (keyPressed SDL.KeycodeSpace)

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event' = case SDL.eventPayload event' of
        SDL.KeyboardEvent keyboardEvent ->
          SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
          SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
        _ -> False

qPressed :: IO Bool
qPressed = Prelude.any eventIsQPress <$> SDL.pollEvents

renderGameState :: SDL.Renderer -> GameState -> IO ()
renderGameState renderer state = do
  drawBackground renderer
  drawState renderer state
  SDL.present renderer

quit :: SDL.Window -> SDL.Renderer -> IO ()
quit window renderer
  = do
    putStrLn "quit"
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

