{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception (bracket)
import Control.Monad (forM_, forever)
import Data.ByteString.Char8 (ByteString, unpack, pack)
import Data.List (findIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import GHC.Conc (atomically, newTVar, readTVar)
import Network.Socket
import Network.Socket.ByteString
import System.Environment (getArgs)

backlog = 1
bufSize = 1024

data Client = Client {clientId :: Int, clientNick :: String, conn :: Socket, addr :: SockAddr}
data Server = Server {hostNick :: TVar String, activeClients :: TVar Int, clients :: TVar (Map Int Client) }

listenUserIn :: (ByteString -> IO ()) -> IO ()
listenUserIn postMessage =
  forever $ (fromString <$> getLine) >>= postMessage

handleMessage :: Socket -> (() -> IO ()) -> (String -> IO ()) -> ([String] -> IO ()) -> IO ()
handleMessage conn quit setNick gotMessage =
  fmap (words . unpack) (recv conn bufSize) >>= go
 where 
  go :: [String] -> IO ()
  go [] = quit ()
  go ("/quit" : _) = quit ()
  go ("/setNick" : nick : _) = setNick nick
  go msg = gotMessage msg >>= \() -> handleMessage conn quit setNick gotMessage

runHost :: Int -> IO ()
runHost port =
  bracket
    ( do
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock (SockAddrInet (fromIntegral port) 0)
        listen sock backlog
        putStrLn $ "Host listening on port " ++ show port
        return sock
    )
    close
    ( \sock -> do
        clients <- atomically $ newTVar Map.empty
        activeClients <- atomically $ newTVar 0
        hostNick <- atomically $ newTVar "host"
        let server = Server{hostNick, activeClients, clients}
        _ <- forkIO $ listenUserIn (postMessage server . words . unpack)
        forever $ do
          (clientSock, clientAddr) <- accept sock
          active <- atomically $ readTVar activeClients
          let client = Client{clientId = active, clientNick = "client " ++ show active, conn = clientSock, addr = clientAddr}
          _ <- atomically $ modifyTVar activeClients succ
          _ <- atomically $ modifyTVar clients (Map.insert (clientId client) client)
          putStrLn $ "Got client: " ++ show clientAddr
          forkIO $ handleClient server client
    )
 where
  postMessage (Server{hostNick, clients, ..}) ("/quit" : _) = do 
    clients' <- atomically $ readTVar clients 
    forM_ (Map.elems clients') (\client -> close (conn client))
  postMessage (Server{hostNick, clients, ..}) ("/setNick" : nick : _) = atomically $ modifyTVar hostNick (const nick)
  postMessage (Server{hostNick, clients, ..}) message = do
    nick <- atomically $ readTVar hostNick
    let withNick = nick ++ ": " ++ unwords message
    clients' <- atomically $ readTVar clients
    forM_ (Map.elems clients') (\client -> sendAll (conn client) (pack withNick))

  handleClient :: Server -> Client -> IO ()
  handleClient server client =
    forever $ do
      let id = clientId client
      handleMessage
        (conn client)
        (removeClient server id)
        (setNick server id)
        (handleClientMessage server client)
  
  removeClient :: Server -> Int -> () -> IO ()
  removeClient server clientId () = do 
    atomically $ modifyTVar (clients server) (Map.delete clientId) 
    atomically $ modifyTVar (activeClients server) pred

  setNick :: Server -> Int -> String -> IO ()
  setNick server clientId nick = atomically $ modifyTVar (clients server) (Map.adjust (\client -> (client{clientNick = nick})) clientId)

  handleClientMessage server client message = do
    let msgWithNick = (clientNick client) ++ ": " ++ unwords message
    putStrLn msgWithNick
    clients <- fmap (Map.elems . Map.delete (clientId client)) $ atomically $ readTVar (clients server)
    forM_ clients (\client -> sendAll (conn client) (pack msgWithNick))

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
    ( \conn -> do
        let post msg = sendAll conn msg
        _ <- forkIO $ listenUserIn post
        handleMessage conn (\() -> putStrLn "disconnected") (const $ return ()) ( putStrLn . unwords )
    )

getMode :: [String] -> Either Int (String, Int)
getMode ("--host" : port : _) = Left $ read port
getMode ("--client" : addr : _) = Right ( uri, port )
 where
  colon = case findIndex ((==) ':') addr of
    Just pt -> pt
    _ -> error "Invalid URI"
  (uri, unparsed) = splitAt colon addr
  port = read $ drop 1 unparsed
getMode _ = error "Invalid arguments expected either \'--host port\' or \'--client uri\'"

main :: IO ()
main = do
  mode <- getMode <$> getArgs
  case mode of
    Left port ->
      runHost port
    Right (uri, port) ->
      runClient uri port
