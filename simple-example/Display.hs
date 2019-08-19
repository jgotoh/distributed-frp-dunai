module Display where

import GameState

import Data.Text
import Foreign.C
import qualified SDL
import SDL.Primitive as SDL

windowWidth :: CInt
windowWidth = 800

windowHeight :: CInt
windowHeight = 600

viewportWidth :: Int
viewportWidth = 1280

viewportHeight :: Int
viewportHeight = 768

createWindow :: String -> CInt -> CInt -> IO SDL.Window
createWindow title width height = do
  let winConfig = SDL.defaultWindow { SDL.windowPosition = SDL.Absolute (SDL.P (SDL.V2 100 100))
                                    , SDL.windowInitialSize = SDL.V2 width height }

  SDL.createWindow (pack title) winConfig

createRenderer :: SDL.Window -> IO SDL.Renderer
createRenderer window = do
  let rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True }
  SDL.createRenderer window (-1) rdrConfig

-- Rendering, TODO maybe use Reader SDL.Renderer

drawBackground :: SDL.Renderer -> IO ()
drawBackground renderer = do
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 100 149 237 255
  SDL.clear renderer

drawState :: SDL.Renderer -> GameState -> IO ()
drawState renderer state = drawCircle renderer $ leftBallPosState state

drawCircle :: SDL.Renderer -> Position -> IO ()
drawCircle renderer = undefined

