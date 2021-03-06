module Input where

import           SDL

keyPressed :: SDL.Keycode -> SDL.Event -> Bool
keyPressed key ev = case SDL.eventPayload ev of
  SDL.KeyboardEvent eData -> case eData of
    SDL.KeyboardEventData _ _ _ ks -> case ks of
      SDL.Keysym _ kc _ -> kc == key
  _ -> False

