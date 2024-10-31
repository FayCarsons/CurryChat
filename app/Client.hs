{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Client (runClient) where

import Common
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TQueue, atomically, newTQueue, tryReadTQueue, writeTQueue)
import Control.Exception (Exception (displayException), IOException, bracket, handle, throwIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket
import System.IO (Handle, hClose, hIsReadable, stdin, stdout)

data Message
  = GotMessage ByteString
  | Close

data Listener
  = Listener (TQueue Message)

sendClient :: TQueue Message -> Message -> IO ()
sendClient queue message = atomically $ writeTQueue queue message

instance Agent Listener where
  getName _ = "Client.hs/Client"
  connection = const stdin
  disconnect (Listener clientQueue) =
    atomically $ writeTQueue clientQueue Close
  setNick (Listener clientQueue) nick = sendClient clientQueue (GotMessage nick)
  showNick (Listener clientQueue) = sendClient clientQueue (GotMessage $ BS.pack "/showNick")
  gotMessage (Listener clientQueue) message = sendClient clientQueue (GotMessage message)

runListener :: Listener -> IO ()
runListener listener =
  Common.runAgent listener >>= \case
    Continue -> runListener listener
    Quit -> return ()

data Client
  = Client
  { conn :: Handle
  , out :: Handle
  , mailbox :: TQueue Message
  }

handleListenerMessage :: Client -> Message -> IO Flow
handleListenerMessage Client{out} (GotMessage message) = BS.hPut out message *> return Continue
handleListenerMessage Client{conn, out} Close =
  BS.hPut conn (BS.pack "/quit")
    *> BS.hPutStrLn out (BS.pack "Closing chat")
    *> hClose conn
    *> return Quit

readConn :: Client -> IO Flow
readConn Client{conn, out} =
  handle ex $
    hIsReadable conn
      >>= ( \case
              True ->
                ( BS.hGetLine conn
                    >>= BS.hPut out
                )
                  *> return Continue
              False -> return Continue
          )
 where
  ex :: IOException -> IO Flow
  ex e = putStrLn (displayException e) *> return Quit

loop :: Client -> IO ()
loop client = do
  readMail client >>= \case
    Just message ->
      handleListenerMessage client message >>= orExit
    Nothing ->
      readConn client >>= orExit
 where
  orExit Continue = loop client
  orExit Quit = return ()
  readMail Client{mailbox} = atomically $ tryReadTQueue mailbox

runClient :: String -> Int -> IO ()
runClient uri port = do
  sock <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints{addrSocketType = Stream}
  addrInfo : _ <- getAddrInfo (Just hints) (Just uri) (Just $ show port)
  let addr = addrAddress addrInfo
  connect sock addr
  putStrLn $ "Connected to host: " ++ show addr
  conn <- socketToHandle sock ReadWriteMode
  mailbox <- atomically $ newTQueue
  _ <- forkIO $ runListener (Listener mailbox)
  loop Client{conn, mailbox, out = stdout}
