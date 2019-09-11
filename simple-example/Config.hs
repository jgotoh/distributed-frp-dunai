module Config where

import Options.Applicative

data Config = Config
  { ipConfig :: String
  , portConfig :: Int
  , createSessionConfig :: Bool } deriving (Show)


configParser :: Parser Config
configParser = Config
  <$> strOption
  (long "ip"
  <> help "IP to join, or where session is created"
  <> metavar "IP")
  <*> option auto
  (long "p"
   <> help "Port of session"
   <> metavar "PORT")
  <*> switch
  (long "host"
   <> help "Create a session"
   <> showDefault
  )

configInfo :: InfoMod Config
configInfo = fullDesc
  <> header "Example Peer-To-Peer application"
  <> progDesc "Start or join a session"

parseConfig :: IO Config
parseConfig = execParser $ info (configParser <**> helper) configInfo
