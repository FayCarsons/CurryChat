{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordPuns #-}

module Host (runHost) where

import Common (Message (..))
import qualified Common
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forM_)
import Control.Monad.Fix (fix)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import GHC.Base (failIO)
import Network.Socket
import System.IO (BufferMode (NoBuffering), Handle, IOMode (ReadWriteMode), hClose, hFlush, hGetLine, hSetBuffering, stdin, stdout)

data MessageSource
  = FromClient Int Message
  | FromHost Message

data Connection
  = Connection
  { conn :: !Handle
  , clientId :: !Int
  , nick :: Maybe ByteString
  }

data Server
  = Server
  { sock :: !Socket
  , hostNick :: !(TVar ByteString)
  , numClients :: !(TVar Int)
  , clients :: !(TVar (IntMap Connection))
  , -- Messages sent to the mailbox are either a bytestring from the Host's stdin,
    -- or a message from a client
    mailbox :: !(TQueue MessageSource)
  }

newServer :: Socket -> IO Server
newServer sock = do
  numClients <- atomically $ newTVar 0
  clients <- atomically $ newTVar Map.empty
  mailbox <- atomically $ newTQueue
  hostNick <- atomically $ newTVar (BS.pack "Host")
  return Server{sock, hostNick, numClients, clients, mailbox}

runHost :: Int -> IO ()
runHost port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral port) 0)
  listen sock Common.backlog
  hSetBuffering stdout NoBuffering
  server <- newServer sock
  _ <- forkIO $ fix $ \loop ->
    hGetLine stdin >>= pure . Common.parseMessage >>= \case
      Just message -> do
        atomically $ writeTQueue (mailbox server) (FromHost message)
        loop
      Nothing ->
        loop
  _ <- forkIO $ fix $ clientListener server
  run server
 where
  clientListener Server{sock, clients, numClients, mailbox} loop = do
    atomically $ modifyTVar numClients succ
    clientId <- atomically $ readTVar numClients
    putStrLn "Accepting a new client"
    (clientSock, addr) <- accept sock
    conn <- socketToHandle clientSock ReadWriteMode
    hSetBuffering conn NoBuffering
    putStrLn $ "Got client: " ++ (show addr)
    let client' = Connection{clientId, conn, nick = Nothing}
    atomically $ modifyTVar clients (Map.insert clientId client')
    _ <- forkIO $ handleClient mailbox client'
    loop
  run server@Server{mailbox} =
    (atomically $ tryReadTQueue mailbox) >>= \case
      Just message -> handleMessage server message *> run server
      Nothing -> do
        run server

broadcast :: Server -> Maybe Int -> ByteString -> IO ()
broadcast Server{clients} except message = do
  clients' <- (atomically $ readTVar clients)
  let withoutSender = Map.elems $ case except of
        Just clientId -> Map.delete clientId clients'
        Nothing -> clients'
  forM_ withoutSender $ \Connection{conn} -> BS.hPutStrLn conn message

handleMessage :: Server -> MessageSource -> IO ()
handleMessage server@Server{hostNick, clients} (FromHost message) =
  case message of
    Quit -> do
      broadcast server Nothing (BS.pack "Host closed room, byebye :P")
      clients' <- Map.elems <$> (atomically $ readTVar clients)
      forM_ clients' $
        \Connection{conn} -> BS.hPutStrLn conn (BS.pack "Host closed room, byebye :P") *> hClose conn
    SetNick nick -> atomically $ modifyTVar hostNick (const nick)
    ShowNick -> (atomically $ readTVar hostNick) >>= BS.putStrLn
    GotMessage msg -> do
      nick <- atomically $ readTVar hostNick
      broadcast server Nothing (BS.concat [nick, BS.pack ": ", msg])
handleMessage server@Server{clients} (FromClient clientId message) =
  case message of
    Quit -> do
      clients' <- (atomically $ readTVar clients)
      Connection{nick, conn} <- maybe (failIO $ "Client " ++ show clientId ++ " not found") pure $ Map.lookup clientId clients'
      msg <- case nick of
        Just n -> return $ BS.concat [n, BS.pack " quit the server"]
        Nothing -> return $ BS.pack $ "User " ++ (show clientId) ++ " quit the server"
      -- Close client conn and remove from client maps
      hClose conn
      atomically $ modifyTVar clients (Map.delete clientId)
      -- Broadcast to other users that client has left
      broadcast server (Just clientId) msg
    SetNick newNick -> do
      clients' <- (atomically $ readTVar clients)
      msg <-
        maybe
          (return $ BS.concat [BS.pack $ "User " ++ (show clientId) ++ " changed their nick to ", newNick])
          (\oldNick -> pure $ BS.concat [BS.pack "Client \'", oldNick, BS.pack "\' changed their nick to \'", newNick, BS.pack "\'"])
          (Map.lookup clientId clients' >>= nick)
      atomically $ modifyTVar clients (Map.adjust (\conn -> conn{nick = Just newNick}) clientId)
      broadcast server (Just clientId) msg
    ShowNick -> do
      clients' <- (atomically $ readTVar clients)
      Connection{nick, conn} <- maybe (failIO $ "Client " ++ show clientId ++ " not found") pure $ Map.lookup clientId clients'
      BS.hPutStrLn conn $ maybe (BS.pack $ "User " ++ show clientId) id nick
    GotMessage msg -> do
      clients' <- (atomically $ readTVar clients)
      withNick <- maybe (pure $ msg) (\nick -> pure $ BS.concat [nick, BS.pack ": ", msg]) (Map.lookup clientId clients' >>= nick)
      BS.putStrLn withNick
      hFlush stdout
      withoutSender <- Map.delete clientId <$> (atomically $ readTVar clients)
      forM_ (Map.elems withoutSender) $ \Connection{conn} -> BS.hPutStrLn conn withNick

handleClient :: TQueue MessageSource -> Connection -> IO ()
handleClient mailbox Connection{conn, clientId} = do
  fix $ \loop -> do
    hGetLine conn >>= pure . Common.parseMessage >>= \case
      Just Quit -> do
        atomically $ writeTQueue mailbox (FromClient clientId Quit)
        return ()
      Just message -> do
        atomically $ (writeTQueue mailbox (FromClient clientId message))
        loop
      Nothing -> loop
