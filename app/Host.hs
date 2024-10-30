{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Host (runHost) where

import Common
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_, forever)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Type.Coercion (trans)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket
import System.IO (BufferMode (NoBuffering), Handle, hClose, hFlush, hSetBuffering, stdin, stdout)

data Client
  = Client
  { clientId :: !Int
  , conn :: !Handle
  , fromServer :: !(TQueue ServerMessage)
  }

{- Can send -}
data ClientMessage
  = GotMessage Int ByteString
  | ChangeNick Int ByteString
  | Leave Int

data Server
  = Server
  { hostNick :: !(TVar String)
  , activeClients :: !(TVar Int)
  , clients :: !(TVar (Map Int Client))
  , clientMeta :: !(TVar (Map Int ByteString))
  , fromClients :: !(TQueue ClientMessage)
  }

{- Can send -}
data ServerMessage
  = SendMessage ByteString ByteString
  | Joined Int
  | ClientLeft ByteString
  | NickChanged ByteString ByteString
  | Announcement ByteString
  | ShutDown

new :: IO Server
new = do
  clients <- atomically $ newTVar Map.empty
  clientMeta <- atomically $ newTVar Map.empty
  activeClients <- atomically $ newTVar 0
  hostNick <- atomically $ newTVar "host"
  fromClients <- atomically newTQueue
  return Server{clients, clientMeta, activeClients, hostNick, fromClients}

getNick :: Int -> TVar (Map Int ByteString) -> IO ByteString
getNick clientId clientMeta = do
  meta <- atomically $ readTVar clientMeta
  case Map.lookup clientId meta of
    Just nick -> return nick
    Nothing -> return "err"

formatBroadcast :: ServerMessage -> ByteString
formatBroadcast (SendMessage nick msg) = BS.concat [nick, BS.pack ": ", msg]
formatBroadcast (Joined clientId) = BS.pack $ "client " ++ show clientId ++ " joined"
formatBroadcast (ClientLeft nick) = BS.concat [nick, BS.pack " left the chat"]
formatBroadcast (NickChanged oldNick newNick) = BS.concat [BS.pack "\'", oldNick, BS.pack "\'", BS.pack " changed their nick to \'", newNick, BS.pack "\'"]
formatBroadcast (Announcement content) = BS.concat [BS.pack "!SERVER ANNOUNCEMENT!\n", BS.pack "*", content, BS.pack "*"]
formatBroadcast ShutDown = BS.pack "Host disconnected"

sendToAll :: [Client] -> ServerMessage -> IO ()
sendToAll clients message = forM_ clients (\Client{fromServer} -> atomically $ writeTQueue fromServer message)

withoutSelf :: Int -> Map Int Client -> [Client]
withoutSelf clientId = Map.elems . Map.delete clientId

handleClientMessage :: Server -> ClientMessage -> IO ()
handleClientMessage Server{clients, clientMeta} (GotMessage clientId msg) = do
  clients' <- atomically $ readTVar clients
  nick <- getNick clientId clientMeta
  sendToAll (withoutSelf clientId clients') (SendMessage nick msg)
handleClientMessage Server{clients, clientMeta} (ChangeNick clientId newNick) = do
  oldNick <- Map.lookup clientId <$> atomically (readTVar clientMeta)
  atomically $ modifyTVar clientMeta (Map.adjust (const newNick) clientId)
  clients' <- atomically $ readTVar clients
  sendToAll (withoutSelf clientId clients') (NickChanged (maybe "err" id oldNick) newNick)
handleClientMessage Server{clients, clientMeta} (Leave clientId) = do
  atomically $ modifyTVar clients (Map.delete clientId)
  nick <- getNick clientId clientMeta
  clients' <- atomically $ readTVar clients
  sendToAll (withoutSelf clientId clients') (ClientLeft nick)

{- Handles clients -}
data Handler = Handler !Client !(TQueue ClientMessage)
instance Agent Handler where
  connection (Handler client _) = conn client
  disconnect (Handler Client{clientId} serverQueue) = atomically $ writeTQueue serverQueue (Leave clientId)
  setNick (Handler Client{clientId} serverQueue) = \nick -> atomically $ writeTQueue serverQueue (ChangeNick clientId (BS.pack nick))
  gotMessage (Handler Client{clientId} serverQueue) = \message -> atomically $ writeTQueue serverQueue (GotMessage clientId (BS.pack message))

{- Handles input from host via stdin -}
data Listener = Listener !Server
instance Agent Listener where
  connection (Listener _) = stdin
  disconnect (Listener Server{hostNick, clients}) = do
    clients' <- atomically $ readTVar clients
    nick <- atomically $ readTVar hostNick
    forM_
      (Map.elems clients')
      ( \Client{clientId, conn} -> do
          BS.hPutStrLn conn (BS.pack $ nick ++ " closed server. goobye :P")
          hFlush conn
          hClose conn
          atomically $ modifyTVar clients (Map.delete clientId)
      )
  setNick (Listener Server{hostNick}) = \nick -> atomically $ writeTVar hostNick nick
  gotMessage (Listener Server{hostNick, clients}) = \message -> do
    nick <- atomically $ readTVar hostNick
    let withNick = nick ++ ": " ++ message
    clients' <- atomically $ readTVar clients
    forM_
      (Map.elems clients')
      ( \Client{conn} -> do
          BS.hPutStrLn conn (BS.pack withNick)
          hFlush conn
      )

runHost :: Int -> IO ()
runHost port =
  bracket start close run
 where
  start = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral port) 0)
    listen sock Common.backlog
    putStrLn $ "Host listening on port " ++ show port
    return sock
  run sock = do
    server <- new
    _ <- forkIO $ Common.runAgent (Listener server)
    forever $ do
      (clientSock, clientAddr) <- accept sock
      atomically $ modifyTVar (activeClients server) succ
      clientId <- atomically $ readTVar (activeClients server)
      let defaultNick = BS.pack $ "client " ++ show clientId
      _ <- atomically $ modifyTVar (clientMeta server) (Map.insert clientId defaultNick)
      putStrLn $ "Got client: " ++ show clientAddr
      forkIO $ spawnClient (fromClients server) clientSock clientId
  spawnClient serverQueue clientSock clientId = do
    conn <- socketToHandle clientSock ReadWriteMode
    hSetBuffering conn NoBuffering
    clientQueue <- atomically $ newTQueue
    let client = Client clientId conn clientQueue
    forever $ Common.runAgent (Handler client serverQueue)
