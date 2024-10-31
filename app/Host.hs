{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Host (runHost) where

import Common
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import Network.Socket
import System.IO (BufferMode (NoBuffering), Handle, hSetBuffering, stdin)

data Client
  = Client
  { clientId :: !Int
  , conn :: !Handle
  , fromServer :: !(TQueue ServerMessage)
  }

data ClientMessage
  = GotMessage Int ByteString
  | ChangeNick Int ByteString
  | ShowNick Int
  | Leave Int

spawnClient :: TQueue ClientMessage -> TQueue ServerMessage -> Socket -> Int -> IO ()
spawnClient serverQueue clientMailbox clientSock clientId = do
  setSocketOption clientSock NoDelay 1
  conn <- socketToHandle clientSock ReadWriteMode
  hSetBuffering conn NoBuffering
  let client = Client clientId conn clientMailbox
  let handler = Handler client serverQueue
  go handler
 where
  go handler =
    checkQueue handler
      *> Common.runAgent handler
      >>= \case
        Continue -> go handler
        Quit -> return ()
  checkQueue handler@(Handler Client{fromServer} _) = do
    atomically (tryReadTQueue fromServer)
      >>= \case
        Just serverMessage -> handleServerMessage handler serverMessage
        Nothing -> return ()

handleServerMessage :: Handler -> ServerMessage -> IO ()
handleServerMessage handler@(Handler client _) message = do
  let fmtMessage = formatBroadcast message
  putStrLn $ "Host.hs/Client " ++ show (clientId client) ++ " got a message: " ++ BS.unpack fmtMessage
  Common.atomicWrite handler fmtMessage *> return ()

formatBroadcast :: ServerMessage -> ByteString
formatBroadcast (SendMessage nick msg) = BS.concat [nick, BS.pack ": ", msg]
formatBroadcast (Joined clientId) = BS.pack $ "client " ++ show clientId ++ " joined"
formatBroadcast (ClientLeft nick) = BS.concat [nick, BS.pack " left the chat"]
formatBroadcast (NickChanged oldNick newNick) = BS.concat [BS.pack "\'", oldNick, BS.pack "\'", BS.pack " changed their nick to \'", newNick, BS.pack "\'"]
formatBroadcast (Announcement content) = BS.concat [BS.pack "!SERVER ANNOUNCEMENT!\n", BS.pack "*", content, BS.pack "*"]
formatBroadcast (ShutDown hostNick) = BS.concat [hostNick, BS.pack "(host) closed server. goodbye :3"]

data Server
  = Server
  { hostConn :: Socket
  , hostNick :: !(TVar ByteString)
  , activeClients :: !(TVar Int)
  , clientMailboxes :: !(TVar (Map Int (TQueue ServerMessage)))
  , clientMeta :: !(TVar (Map Int ByteString))
  , fromClients :: !(TQueue ClientMessage)
  }

data ServerMessage
  = SendMessage ByteString ByteString
  | Joined Int
  | ClientLeft ByteString
  | NickChanged ByteString ByteString
  | Announcement ByteString
  | ShutDown ByteString

new :: Socket -> IO Server
new hostConn = do
  clientMailboxes <- atomically $ newTVar Map.empty
  clientMeta <- atomically $ newTVar Map.empty
  activeClients <- atomically $ newTVar 0
  hostNick <- atomically $ newTVar "host"
  fromClients <- atomically newTQueue
  return Server{hostConn, clientMailboxes, clientMeta, activeClients, hostNick, fromClients}

getNick :: Int -> TVar (Map Int ByteString) -> IO ByteString
getNick clientId clientMeta = do
  meta <- atomically $ readTVar clientMeta
  case Map.lookup clientId meta of
    Just nick -> return nick
    Nothing -> return "err"

sendToAll :: [TQueue ServerMessage] -> ServerMessage -> IO ()
sendToAll clients message = forM_ clients (\q -> atomically $ writeTQueue q message)

withoutSelf :: Int -> Map Int (TQueue ServerMessage) -> [TQueue ServerMessage]
withoutSelf clientId = Map.elems . Map.delete clientId

handleClientMessage :: Server -> ClientMessage -> IO ()
handleClientMessage Server{hostNick, clientMailboxes, clientMeta} message =
  case message of
    GotMessage clientId msg -> do
      clients' <- atomically $ readTVar clientMailboxes
      nick <- getNick clientId clientMeta
      sendToAll (withoutSelf clientId clients') (SendMessage nick msg)
    ChangeNick clientId newNick -> do
      oldNick <- getNick clientId clientMeta
      atomically $ modifyTVar clientMeta (Map.adjust (const newNick) clientId)
      mailBoxes <- atomically $ readTVar clientMailboxes
      sendToAll (withoutSelf clientId mailBoxes) (NickChanged oldNick newNick)
    ShowNick clientId -> do
      nick <- getNick clientId clientMeta
      hostNick' <- atomically $ readTVar hostNick
      mailboxes <- atomically $ readTVar clientMailboxes
      case Map.lookup clientId mailboxes of
        Just mailbox -> atomically $ writeTQueue mailbox (SendMessage hostNick' nick)
        Nothing -> undefined
    Leave clientId -> do
      nick <- getNick clientId clientMeta

      atomically $ modifyTVar clientMailboxes (Map.delete clientId)
      atomically $ modifyTVar clientMeta (Map.delete clientId)
      mailboxes <- atomically $ readTVar clientMailboxes
      sendToAll (withoutSelf clientId mailboxes) (ClientLeft nick)

{- Handles clients -}
data Handler = Handler !Client !(TQueue ClientMessage)
instance Agent Handler where
  getName _ = "Host.hs/Handler"
  connection (Handler client _) = conn client
  disconnect (Handler Client{clientId} serverQueue) = atomically $ writeTQueue serverQueue (Leave clientId)
  setNick (Handler Client{clientId} serverQueue) = \nick -> atomically $ writeTQueue serverQueue (ChangeNick clientId nick)
  showNick (Handler Client{clientId} serverQueue) = atomically $ writeTQueue serverQueue (ShowNick clientId)
  gotMessage (Handler Client{clientId} serverQueue) = \message -> atomically $ writeTQueue serverQueue (GotMessage clientId message)

{- Handles input from host via stdin -}
data Listener = Listener !Server
instance Agent Listener where
  getName _ = "Host.hs/Listener"
  connection (Listener _) = stdin
  disconnect (Listener Server{hostNick, clientMailboxes}) = do
    clients' <- atomically $ readTVar clientMailboxes
    nick <- atomically $ readTVar hostNick
    forM_
      (Map.elems clients')
      (\q -> atomically $ writeTQueue q (ShutDown nick))
  setNick (Listener Server{hostNick}) = atomically . writeTVar hostNick
  showNick (Listener Server{hostNick}) = do
    nick <- atomically $ readTVar hostNick
    BS.hPutStrLn stdin nick
  gotMessage (Listener Server{hostNick, clientMailboxes}) = \message -> do
    nick <- atomically $ readTVar hostNick
    mailboxes <- Map.elems <$> atomically (readTVar clientMailboxes)
    sendToAll mailboxes (SendMessage nick message)

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
    server <- new sock
    _ <- forkIO $ loopListener (Listener server)
    loopServer server
  loopListener :: Listener -> IO ()
  loopListener listener =
    Common.runAgent listener
      >>= \case
        Continue -> loopListener listener
        Quit -> return ()
  loopServer :: Server -> IO ()
  loopServer server@Server{hostConn, clientMailboxes, clientMeta, fromClients, activeClients} =
    atomically (tryReadTQueue fromClients)
      >>= \case
        Just message -> handleClientMessage server message *> loopServer server
        Nothing -> do
          (clientSock, clientAddr) <- accept hostConn
          putStrLn $ "Got client: " ++ show clientAddr

          atomically $ modifyTVar activeClients succ
          clientId <- atomically $ readTVar activeClients
          let defaultNick = BS.pack $ "client " ++ show clientId
          _ <- atomically $ modifyTVar clientMeta (Map.insert clientId defaultNick)
          clientMailbox <- atomically $ newTQueue
          atomically $ modifyTVar clientMailboxes (Map.insert clientId clientMailbox)
          forkIO (spawnClient fromClients clientMailbox clientSock clientId) *> loopServer server
