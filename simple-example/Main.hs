module Main where

import Display
import Network.Connection
import Time

import Control.Monad
import Data.Functor.Identity
import Data.IORef
import FRP.BearRiver
import qualified SDL

main :: IO ()
main = do
  (window, renderer) <- initializeSDL
  timeRef <- createTimeRef

  SDL.showWindow window
  reactimate (return ()) (sense timeRef) (\_ e -> renderGameState renderer e >> qPressed) gameSF

  quit window renderer

initializeSDL :: IO (SDL.Window, SDL.Renderer)
initializeSDL = do
  SDL.initializeAll
  window <- createWindow "simple-example" windowWidth windowHeight
  renderer <- createRenderer window
  return (window, renderer)

sense ::IORef DTime -> Bool -> IO (DTime, Maybe ())
sense timeRef _ = do
  dtMillis <- senseTime timeRef
  return (dtMillis, Just ())

gameSF :: MSF (ClockInfo Data.Functor.Identity.Identity) () ()
gameSF = arr $ return ()

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event' = case SDL.eventPayload event' of
        SDL.KeyboardEvent keyboardEvent ->
          SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
          SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
        _ -> False

qPressed :: IO Bool
qPressed = do
  events <- SDL.pollEvents
  return $ any eventIsQPress events

renderGameState :: SDL.Renderer -> () -> IO ()
renderGameState renderer _ = do
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 100 149 237 255
  SDL.clear renderer
  SDL.present renderer

quit :: SDL.Window -> SDL.Renderer -> IO ()
quit window renderer
  = do
    putStrLn "quit"
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

