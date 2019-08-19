module Display where

import GameState

import Data.Text
import Foreign.C
import qualified SDL
import SDL.Vect
import SDL.Primitive as SDL

playerRadius :: Double
playerRadius = 30

windowWidth :: CInt
windowWidth = 800

windowHeight :: CInt
windowHeight = 600

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
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 100 149 237 255 -- #6495ed
  SDL.clear renderer

drawState :: SDL.Renderer -> GameState -> IO ()
drawState renderer state = drawCircle renderer $ leftBallPosState state

drawCircle :: SDL.Renderer -> Position -> IO ()
drawCircle renderer pos = SDL.smoothEllipse renderer (subtractWindowHeight (round <$> pos)) (round playerRadius) (round playerRadius) color
    where
      color = SDL.V4 240 142 125 255

subtractWindowHeight :: V2 CInt -> V2 CInt
subtractWindowHeight v = case v of
  V2 x y -> V2 x (windowHeight - y)

