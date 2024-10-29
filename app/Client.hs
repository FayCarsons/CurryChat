{-# LANGUAGE NamedFieldPuns #-}

module Client (runClient) where

import Common
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (NoBuffering), Handle, hFlush, hSetBuffering)

data Client = Client !Handle
instance Agent Client where
  connection (Client conn) = conn
  disconnect _ = return ()
  setNick _ = const $ return ()
  gotMessage (Client conn) = \message -> do
    BS.hPutStrLn conn $ BS.pack message
    hFlush conn

runClient :: String -> Int -> IO ()
runClient uri port =
  bracket
    ( do
        sock <- socket AF_INET Stream defaultProtocol
        let hints = defaultHints{addrSocketType = Stream}
        addr : _ <- getAddrInfo (Just hints) (Just uri) (Just $ show port)
        connect sock $ addrAddress addr
        return sock
    )
    close
    ( \sock -> do
        conn <- socketToHandle sock ReadWriteMode
        hSetBuffering conn NoBuffering

        let postMessage = \message -> do
              BS.hPutStrLn conn message
              hFlush conn
        Common.runListener postMessage

        Common.runAgent (Client conn)
    )
