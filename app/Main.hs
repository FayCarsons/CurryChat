module Main where

import Client (runClient)
import Data.List (findIndex)
import Host (runHost)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Mode
  = Host Int
  | Client String Int
  | Usage
data Error = InvalidURI | InvalidPort | UnexpectedArg

usage :: String
usage =
  unlines
    [ "OPTIONS:"
    , "[--host port] start application as a host"
    , "[--client addr:port] start application as a client, connect to a host"
    , "[--help] print this message"
    ]

main :: IO ()
main = getArgs >>= go . getMode
 where
  go (Left (InvalidURI, uri)) = putStrLn $ "Received invalid URI \'" ++ uri ++ "\'"
  go (Left (InvalidPort, port)) = putStrLn $ "Cannot parse port \'" ++ port ++ "\'"
  go (Left (UnexpectedArg, args)) = do
    putStrLn $ "Unexpected argument(s): \'" ++ args ++ "\'"
    putStrLn usage
  go (Right Usage) = putStrLn usage
  go (Right (Host port)) = runHost port
  go (Right (Client uri port)) = runClient uri port

getMode :: [String] -> Either (Error, String) Mode
getMode ("--help" : _) = Right Usage
getMode ("--host" : unparsedPort : _) =
  case readMaybe unparsedPort of
    Just port -> Right $ Host port
    Nothing -> Left (InvalidPort, unparsedPort)
getMode ("--client" : addr : _) = do
  colon <- case findIndex ((==) ':') addr of
    Just idx -> Right idx
    Nothing -> Left (InvalidURI, addr)
  let (uri, unparsedPort) = splitAt colon addr
  case readMaybe $ drop 1 unparsedPort of
    Just port -> return $ Client uri port
    Nothing -> Left (InvalidPort, unparsedPort)
getMode args = Left (UnexpectedArg, foldr (++) "" args)
