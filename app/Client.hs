{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Client (runClient) where

import Control.Concurrent (forkIO)
import Control.Monad.Fix (fix)
import qualified Data.ByteString.Char8 as BS
import Network.Socket
import System.Exit (exitSuccess)
import System.IO

runClient :: String -> Int -> IO ()
runClient uri port = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints{addrSocketType = Stream}
  addr : _ <- getAddrInfo (Just hints) (Just uri) (Just $ show port)
  connect sock $ addrAddress addr
  putStrLn $ "Connected to host: " ++ show (addrAddress addr)
  conn <- socketToHandle sock ReadWriteMode
  hSetBuffering conn NoBuffering
  let postMessage = BS.hPutStrLn conn
  _ <- forkIO $ fix (runListener conn postMessage)
  fix $ handleConn conn
 where
  -- Get input from stdin, if it's 'quit' then forward to host and close app
  runListener conn postMessage continue =
    getLine >>= \case
      "/quit" -> do
        _ <- postMessage (BS.pack "/quit")
        hClose conn
        exitSuccess
      msg -> postMessage (BS.pack msg) *> continue
  handleConn conn continue = do
    line <- BS.hGetLine conn
    if BS.null line
      then
        putStrLn "Connection closed :3"
      else BS.putStrLn line *> continue
