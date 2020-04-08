module Config where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

data DRMConfig = DRMZero
               | DRMFirst
               deriving Show

data Config = ClientConfig { ipConfig :: String
  , portConfig :: Int
  , nickConfig :: String
  , nameConfig :: String
  , serverAddrConfig :: String
  , useCSPConfig :: Bool
  , drmConfig :: DRMConfig }
  | ServerConfig { ipConfig :: String
  , portConfig :: Int
  , nameConfig :: String
  , lengthConfig :: Int
  , useTimeWarpConfig :: Bool}
  | GameConfig
  deriving (Show)

clientConfigParser :: Parser Config
clientConfigParser =
  ClientConfig
    <$> strOption (long "ip" <> help "IP of client" <> metavar "IP")
    <*> option auto (long "p" <> help "Port of client" <> metavar "PORT")
    <*> strOption (long "nick" <> help "Nickname of Client" <> metavar "NICK")
    <*> strOption
          (long "name" <> help "Name of server session to join" <> metavar
            "NAME"
          )
    <*> strOption
          (long "s" <> help "EndpointAddress of Server to join" <> metavar
            "ADDRESS"
          )
    <*> switch (long "csp" <> short 'c' <> help "whether to use client side prediction of the locally controlled paddle")
    <*> flag DRMZero DRMFirst (long "drmFirst" <> short 'd' <> help "Enable first order dead reckoning")

serverConfigParser :: Parser Config
serverConfigParser =
  flag' ServerConfig (long "host" <> help "host server")
    <*> strOption (long "ip" <> help "IP of server" <> metavar "IP")
    <*> option auto (long "p" <> help "Port of server" <> metavar "PORT")
    <*> strOption (long "name" <> help "Name of session" <> metavar "NAME")
    <*> option auto (long "d" <> help "Length of round in seconds" <> metavar "LENGTH")
    <*> switch
          (long "timewarp" <> short 't' <> help
            "Whether to use time warp synchronisation"
          )

gameConfigParser :: Parser Config
gameConfigParser = flag'
  GameConfig
  (long "game" <> help "start the simulation without any networking features")

configParser :: Parser Config
configParser = gameConfigParser <|> clientConfigParser <|> serverConfigParser

configInfo :: InfoMod Config
configInfo =
  fullDesc <> header "Example Client/ Server application" <> progDesc
    "Start or join a session"

parseConfig :: IO Config
parseConfig = execParser $ info (configParser <**> helper) configInfo

