module Main where

import Display
import Game
import GameState
import Input
import Time

import Data.IORef
import FRP.BearRiver
import qualified SDL

main :: IO ()
main = do
  (window, renderer) <- initializeSDL
  timeRef <- createTimeRef

  SDL.showWindow window
  --reactimate :: Monad m => m a -> (Bool -> m (DTime, Maybe a)) -> (Bool -> b -> m Bool) -> SF Identity a b -> m ()
  reactimate (return GameInput) (sense timeRef) (actuate renderer) gameSF

  quit window renderer

initializeSDL :: IO (SDL.Window, SDL.Renderer)
initializeSDL = do
  SDL.initializeAll
  window <- createWindow "simple-example" windowWidth windowHeight
  renderer <- createRenderer window
  return (window, renderer)

actuate :: SDL.Renderer -> p -> GameState -> IO Bool
actuate renderer _ state = renderGameState renderer state >> qPressed

sense ::IORef DTime -> Bool -> IO (DTime, Maybe GameInput)
sense timeRef _ = do
  dtMillis <- senseTime timeRef
  return (dtMillis, Just GameInput)

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event' = case SDL.eventPayload event' of
        SDL.KeyboardEvent keyboardEvent ->
          SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
          SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
        _ -> False

qPressed :: IO Bool
qPressed = do
  events <- SDL.pollEvents
  return $ Prelude.any eventIsQPress events

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

