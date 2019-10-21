module Display where

import           Types
import           Data.Text
import           Foreign.C
import qualified SDL
import           SDL.Vect
import qualified SDL.Primitive                 as SDL

playerRadius :: Double
playerRadius = 15

windowWidth :: CInt
windowWidth = 400

windowHeight :: CInt
windowHeight = 250

initializeSDL :: String -> IO (SDL.Window, SDL.Renderer)
initializeSDL title = do
  SDL.initializeAll
  window   <- createWindow title windowWidth windowHeight
  renderer <- createRenderer window
  return (window, renderer)

createWindow :: String -> CInt -> CInt -> IO SDL.Window
createWindow title width height = do
  let winConfig = SDL.defaultWindow
        { SDL.windowPosition    = SDL.Absolute (SDL.P (SDL.V2 100 100))
        , SDL.windowInitialSize = SDL.V2 width height
        }

  SDL.createWindow (pack title) winConfig

createRenderer :: SDL.Window -> IO SDL.Renderer
createRenderer window = do
  let rdrConfig = SDL.RendererConfig
        { SDL.rendererType          = SDL.AcceleratedVSyncRenderer
        , SDL.rendererTargetTexture = True
        }
  SDL.createRenderer window (-1) rdrConfig

-- Rendering, TODO maybe use Reader SDL.Renderer

drawBackground :: SDL.Renderer -> IO ()
drawBackground renderer = do
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 100 149 237 255
  SDL.clear renderer


drawCircle :: SDL.Renderer -> Position -> IO ()
drawCircle renderer pos = do
  SDL.smoothEllipse renderer
                    (sdlpos pos)
                    (round playerRadius)
                    (round playerRadius)
                    color
  SDL.fillEllipse renderer
                  (sdlpos pos)
                  (round playerRadius)
                  (round playerRadius)
                  color
 where
  color  = SDL.V4 240 142 125 255

sdlpos pos = subtractWindowHeight (round <$> pos)

drawRect :: SDL.Renderer -> Position -> Color -> IO ()
drawRect r pos = do
  SDL.fillRectangle r (sdlpos pos) (sdlpos $ pos ^+^ (V2 10 50))

subtractWindowHeight :: V2 CInt -> V2 CInt
subtractWindowHeight v = case v of
  V2 x y -> V2 x (windowHeight - y)

quit :: SDL.Window -> SDL.Renderer -> IO ()
quit window renderer = do
  putStrLn "quit"
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

