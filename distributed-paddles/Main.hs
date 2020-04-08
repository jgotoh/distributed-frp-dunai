module Main where

import           System.IO
import           Config
import           ClientMain
import           ServerMain

main :: IO ()
main = do
  -- flush output on every newline to support output stream editing via sed
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering

  cfg <- parseConfig

  print cfg

  case cfg of
    ClientConfig ip port nick name server csp drm ->
      clientMain ip port nick name server csp drm
    ServerConfig ip port name roundLength useTW -> serverMain ip port name roundLength useTW
    GameConfig                -> undefined
