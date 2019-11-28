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
    ClientConfig ip port nick name server ->
      clientMain ip port nick name server
    ServerConfig ip port name -> serverMain ip port name
    GameConfig                -> undefined
