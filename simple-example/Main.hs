module Main where

import Display
import Network.Connection

import Control.Monad
import qualified SDL

main :: IO ()
main = do
  (window, renderer) <- initializeGame
  SDL.showWindow window
  putStrLn "hello world"
  appLoop renderer
  quit window renderer

initializeGame :: IO (SDL.Window, SDL.Renderer)
initializeGame = do
  SDL.initializeAll

  window <- createWindow "simple-example" windowWidth windowHeight
  renderer <- createRenderer window
  return (window, renderer)

appLoop :: SDL.Renderer -> IO ()
appLoop renderer = do
  events <- SDL.pollEvents

  let eventIsQPress event = case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent ->
          SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
          SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
        _ -> False

  let qPressed = any eventIsQPress events

  SDL.rendererDrawColor renderer SDL.$= SDL.V4 100 149 237 255
  SDL.clear renderer
  SDL.present renderer
  unless qPressed (appLoop renderer)

quit :: SDL.Window -> SDL.Renderer -> IO ()
quit window renderer
  = do
    putStrLn "quit"
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
