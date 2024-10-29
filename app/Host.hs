{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Host (runHost) where

import Common
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_, forever)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Network.Socket
import Network.Socket.ByteString (sendAll)

data Client = Client {clientId :: Int, clientNick :: String, conn :: Socket, addr :: SockAddr}
data Server = Server {hostNick :: TVar String, activeClients :: TVar Int, clients :: TVar (Map Int Client)}
data Handler = Handler Client Server
instance Agent Handler where
  connection (Handler client _) = conn client
  disconnect (Handler client Server{clients, activeClients}) = do
    atomically $ modifyTVar clients (Map.delete $ clientId client)
    atomically $ modifyTVar activeClients pred
  setNick (Handler client server) = \nick ->
    atomically $ modifyTVar (clients server) (Map.adjust (\c -> c{clientNick = nick}) (clientId client))
  gotMessage (Handler client Server{clients}) = \message -> do
    do
      let msgWithNick = BS.pack $ (clientNick client) ++ ": " ++ message
      BS.putStrLn msgWithNick
      withoutSelf <- fmap (Map.elems . Map.delete (clientId client)) $ atomically $ readTVar clients
      forM_ withoutSelf (\c -> sendAll (conn c) msgWithNick)

runHost :: Int -> IO ()
runHost port =
  bracket
    ( do
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock (SockAddrInet (fromIntegral port) 0)
        listen sock Common.backlog
        putStrLn $ "Host listening on port " ++ show port
        return sock
    )
    close
    ( \sock -> do
        clients <- atomically $ newTVar Map.empty
        activeClients <- atomically $ newTVar 0
        hostNick <- atomically $ newTVar "host"
        let server = Server{hostNick, activeClients, clients}
        _ <- forkIO $ Common.listenUserIn (postMessage server . words . BS.unpack)
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
  handleClient :: Server -> Client -> IO ()
  handleClient server client =
    forever $ do
      Common.handleMessage
        (Handler client server)

  postMessage :: Server -> [String] -> IO ()
  postMessage (Server{clients}) ("/quit" : _) = do
    clients' <- atomically $ readTVar clients
    forM_ (Map.elems clients') (\client -> close (conn client))
  postMessage (Server{hostNick}) ("/setNick" : nick : _) = atomically $ modifyTVar hostNick (const nick)
  postMessage (Server{hostNick, clients}) message = do
    nick <- atomically $ readTVar hostNick
    let withNick = nick ++ ": " ++ unwords message
    clients' <- atomically $ readTVar clients
    forM_ (Map.elems clients') (\client -> sendAll (conn client) (BS.pack withNick))
