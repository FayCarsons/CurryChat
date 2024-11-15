{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Client
import qualified Common
import Data.List (elemIndex)
import qualified Host
import Options.Applicative
import Text.Read (readEither)

data Mode = Host Int | Client String Int

host :: Parser Mode
host =
  Host
    <$> option
      (eitherReader readEither)
      (long "host" <> short 'h' <> metavar "port" <> help "Start the application in host mode running on specified port")

client :: Parser Mode
client =
  option
    parseClientParams
    (long "client" <> short 'c' <> metavar "host uri" <> help "Start the application in client mode, connecting to the specified URI")
 where
  parseClientParams :: ReadM Mode
  parseClientParams =
    eitherReader
      ( \s ->
          case elemIndex ':' s of
            Just splitPoint ->
              let (uri, unparsedPort) = splitAt splitPoint s
               in readEither (drop 1 unparsedPort) >>= \port -> Right (Client uri port)
            Nothing -> Left "Invalid port (no colon) - expected a URI like: \'127.0.0.1:8080\'"
      )
mode :: ParserInfo Mode
mode = info (host <|> client) (fullDesc <> progDesc "A concurrent chat server written in Haskell" <> header "CurryChat")

main :: IO ()
main =
  Common.clearScreen
    *> putStrLn Common.appHeader
    *> execParser mode
    >>= \case
      Host port -> Host.runHost port
      Client addr port -> Client.runClient addr port
