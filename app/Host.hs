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
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket
import Network.Socket.ByteString (sendAll)
import System.IO (stdin)

data Client
  = Client
  { clientId :: !Int
  , clientNick :: !String
  , conn :: !Socket
  }

data Server
  = Server
  { hostNick :: !(TVar String)
  , activeClients :: !(TVar Int)
  , clients :: !(TVar (Map Int Client))
  }

{- Handles clients -}
data Handler = Handler !Client !Server
instance Agent Handler where
  connection (Handler client _) = socketToHandle (conn client) ReadWriteMode
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

{- Handles input from host via stdin -}
data Listener = Listener !Server
instance Agent Listener where
  connection (Listener _) = return stdin
  disconnect (Listener Server{hostNick, clients}) = do
    clients' <- atomically $ readTVar clients
    nick <- atomically $ readTVar hostNick
    forM_
      (Map.elems clients')
      ( \client -> do
          sendAll (conn client) (BS.pack $ nick ++ " closed server. goobye :P")
          close (conn client)
      )
  setNick (Listener Server{hostNick}) = \nick -> atomically $ writeTVar hostNick nick
  gotMessage (Listener Server{hostNick, clients}) = \message -> do
    nick <- atomically $ readTVar hostNick
    let withNick = nick ++ ": " ++ message
    clients' <- atomically $ readTVar clients
    forM_ (Map.elems clients') (\client -> sendAll (conn client) (BS.pack withNick))

runHost :: Int -> IO ()
runHost port =
  bracket
    start
    close
    run
 where
  start = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral port) 0)
    listen sock Common.backlog
    putStrLn $ "Host listening on port " ++ show port
    return sock
  run sock = do
    clients <- atomically $ newTVar Map.empty
    activeClients <- atomically $ newTVar 0
    hostNick <- atomically $ newTVar "host"
    let server = Server{hostNick, activeClients, clients}
    _ <- forkIO $ Common.runAgent (Listener server)
    forever $ do
      (clientSock, clientAddr) <- accept sock
      atomically $ modifyTVar activeClients succ
      active <- atomically $ readTVar activeClients
      let client =
            Client
              { clientId = active
              , clientNick = "client " ++ show active
              , conn = clientSock
              }
      _ <- atomically $ modifyTVar clients (Map.insert (clientId client) client)
      putStrLn $ "Got client: " ++ show clientAddr
      forkIO . forever $ Common.runAgent (Handler client server)
