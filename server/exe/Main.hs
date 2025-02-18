{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Api (app)
import Cardano.Api qualified as C
import Control.Applicative ((<**>))
import Data.Function ((&))
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import Network.Wai.Handler.Warp (
  Port,
  Settings,
  defaultSettings,
  runSettings,
  setLogger,
  setPort,
 )
import Network.Wai.Logger (withStdoutLogger)
import Ogmios.Query qualified
import Options.Applicative qualified as Options
import System.Exit (die)
import Text.Read (readMaybe)
import Types (ServerOptions (ServerOptions, port), newEnvIO)

main :: IO ()
main = do
  serverOptions@ServerOptions {port} <- Options.execParser opts
  withStdoutLogger $ \logger -> do
    putStrLn $ "CTL server starting on port " <> show port
    runSettings (mkSettings port logger)
      . app
      =<< either die pure
      =<< newEnvIO serverOptions
  where
    mkSettings ::
      Port -> (Request -> Status -> Maybe Integer -> IO ()) -> Settings
    mkSettings port logger = defaultSettings & setPort port & setLogger logger

opts :: Options.ParserInfo ServerOptions
opts =
  Options.info (serverOptionsParser <**> Options.helper) $
    Options.fullDesc
      <> Options.progDesc
        "CBTx server. See the README for routes and request/response types"

networkIdReader :: Options.ReadM C.NetworkId
networkIdReader = Options.eitherReader $ \case
  "mainnet" -> pure C.Mainnet
  networkMagicStr ->
    maybe
      (Left "Failed to parse network ID")
      (pure . C.Testnet . C.NetworkMagic)
      $ readMaybe networkMagicStr

serverOptionsParser :: Options.Parser ServerOptions
serverOptionsParser =
  ServerOptions
    <$> Options.option
      Options.auto
      ( Options.long "port"
          <> Options.short 'p'
          <> Options.help "Server port"
          <> Options.showDefault
          <> Options.value 8081
          <> Options.metavar "INT"
      )
    <*> Options.option
      Options.str
      ( Options.long "node-socket"
          <> Options.short 's'
          <> Options.help "Cardano Node socket path"
          <> Options.showDefault
          <> Options.value "./node/socket/node.socket"
          <> Options.metavar "PATH"
      )
    <*> Options.option
      networkIdReader
      ( Options.long "network-id"
          <> Options.help "Network id: either 'mainnet' or a NetworkMagic number"
          <> Options.showDefault
          <> Options.value C.Mainnet
          <> Options.metavar "NETWORKID"
      )
    <*> Options.option
      Options.str
      ( Options.long "ogmios-host"
          <> Options.help "The hostname for ogmios"
          <> Options.showDefault
          <> Options.value
            ( Ogmios.Query.host
                Ogmios.Query.defaultServerParameters
            )
          <> Options.metavar "IPV4"
      )
    <*> Options.option
      Options.auto
      ( Options.long "ogmios-port"
          <> Options.help "The port for ogmios"
          <> Options.showDefault
          <> Options.value
            ( Ogmios.Query.port
                Ogmios.Query.defaultServerParameters
            )
          <> Options.metavar "INT"
      )
