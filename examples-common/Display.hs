module Display where

import           Types
import           Data.Text
import           Foreign.C
import qualified SDL
import           SDL.Vect
import qualified SDL.Primitive                 as SDL

windowWidth :: Int
windowWidth = 400

windowHeight :: Int
windowHeight = 250

cint :: Int -> CInt
cint = CInt . fromIntegral

initializeSDL :: String -> IO (SDL.Window, SDL.Renderer)
initializeSDL title = do
  SDL.initializeAll
  window   <- createWindow title (cint windowWidth) (cint windowHeight)
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


drawCircle :: SDL.Renderer -> Position -> Radius -> IO ()
drawCircle renderer pos radius = do
  SDL.smoothEllipse renderer
                  (cint <$> sdlpos pos)
                  (round radius)
                  (round radius)
                  color
  SDL.fillEllipse renderer
                  (cint <$> sdlpos pos)
                  (round radius)
                  (round radius)
                  color
 where
  color  = SDL.V4 240 142 125 255

sdlpos :: V2 Double -> V2 Int
sdlpos pos = subtractWindowHeight (round <$> pos)

drawRect :: SDL.Renderer -> Position -> Bounds -> Color -> IO ()
drawRect r pos bounds = do
  SDL.fillRectangle r (cint <$> sdlpos pos) (cint <$> (sdlpos $ pos ^+^ bounds))

subtractWindowHeight :: V2 Int -> V2 Int
subtractWindowHeight v = case v of
  V2 x y -> V2 x (windowHeight - y)

quit :: SDL.Window -> SDL.Renderer -> IO ()
quit window renderer = do
  putStrLn "quit"
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

