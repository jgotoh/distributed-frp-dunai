module Config where

import Options.Applicative
import Data.Semigroup ((<>))

data Config = ClientConfig { ipConfig :: String
  , portConfig :: Int
  , nickConfig :: String
  , nameConfig :: String
  , serverAddrConfig :: String }
  | ServerConfig { ipConfig :: String
  , portConfig :: Int
  , nameConfig :: String}
  | GameConfig
  deriving (Show)

clientConfigParser :: Parser Config
clientConfigParser = ClientConfig
  <$> strOption
  (long "ip"
  <> help "IP of client"
  <> metavar "IP")
  <*> option auto
  (long "p"
   <> help "Port of client"
   <> metavar "PORT")
  <*> strOption
  (long "nick"
   <> help "Nickname of Client"
   <> metavar "NICK")
  <*> strOption
  (long "name"
   <> help "Name of server session to join"
   <> metavar "NAME")
  <*> strOption
  (long "s"
   <> help "EndpointAddress of Server to join"
   <> metavar "ADDRESS")

serverConfigParser :: Parser Config
serverConfigParser = flag' ServerConfig ( long "host" <> help "host server")
  <*> strOption
  (long "ip"
  <> help "IP of server"
  <> metavar "IP")
  <*> option auto
  (long "p"
   <> help "Port of server"
   <> metavar "PORT")
  <*> strOption
  (long "name"
   <> help "Name of session"
   <> metavar "NAME")

gameConfigParser :: Parser Config
gameConfigParser = flag' GameConfig (long "game" <> help "start the simulation without any networking features")

configParser :: Parser Config
configParser = gameConfigParser <|> clientConfigParser <|> serverConfigParser

configInfo :: InfoMod Config
configInfo = fullDesc
  <> header "Example Client/ Server application"
  <> progDesc "Start or join a session"

parseConfig :: IO Config
parseConfig = execParser $ info (configParser <**> helper) configInfo

