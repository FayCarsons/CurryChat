{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Client (runClient) where

import Control.Concurrent (forkIO)
import Control.Monad.Fix (fix)
import qualified Data.ByteString.Char8 as BS
import Network.Socket
import System.IO

runClient :: String -> Int -> IO ()
runClient uri port = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints{addrSocketType = Stream}
  addr : _ <- getAddrInfo (Just hints) (Just uri) (Just $ show port)
  connect sock $ addrAddress addr
  putStrLn $ "Connected to host: " ++ (show $ addrAddress addr)
  conn <- socketToHandle sock ReadWriteMode
  hSetBuffering conn NoBuffering
  let postMessage msg = BS.hPutStrLn conn msg
  _ <- forkIO $ fix $ \loop ->
    getLine >>= \case
      "/quit" -> do
        postMessage (BS.pack "/quit")
        hClose conn
        return ()
      msg -> postMessage (BS.pack msg) *> loop
  fix $ handleConn conn
 where
  handleConn conn continuation = do
    line <- BS.hGetLine conn
    if BS.null line
      then
        putStrLn "Connection closed :3"
      else BS.putStrLn line *> continuation
