{-# LANGUAGE NamedFieldPuns #-}

module Client (runClient) where

import Common
import Control.Exception (bracket)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket
import Network.Socket.ByteString

data Client = Client !Socket
instance Agent Client where
  connection (Client sock) = socketToHandle sock ReadWriteMode
  disconnect _ = return ()
  setNick _ = const $ return ()
  gotMessage _ = putStrLn

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
        let postMessage = sendAll sock
        Common.listenUserIn postMessage
        Common.runAgent (Client sock)
    )
