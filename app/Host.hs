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
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Base (failIO)
import Network.Socket
import System.IO (BufferMode (NoBuffering), Handle, IOMode (ReadWriteMode), hClose, hFlush, hGetLine, hSetBuffering, stdin, stdout)

data MessageSource
  = FromClient Int Message
  | FromHost Message

data Connection
  = Connection
  { conn :: !Handle
  , clientId :: Int
  }

data Server
  = Server
  { sock :: !Socket
  , hostNick :: !(TVar ByteString)
  , numClients :: !(TVar Int)
  , clients :: !(TVar (Map Int Connection))
  , -- Client metadata is currently just client's nicknames, but in the future it
    -- could be time active, or a rate-limiting variable
    clientMetadata :: !(TVar (Map Int (Maybe ByteString)))
  , -- Messages sent to the mailbox are either a bytestring from the Host's stdin,
    -- or a message from a client
    mailbox :: !(TQueue MessageSource)
  }

newServer :: Socket -> IO Server
newServer sock = do
  numClients <- atomically $ newTVar 0
  clients <- atomically $ newTVar Map.empty
  clientMetadata <- atomically $ newTVar Map.empty
  mailbox <- atomically $ newTQueue
  hostNick <- atomically $ newTVar (BS.pack "Host")
  return Server{sock, hostNick, numClients, clients, clientMetadata, mailbox}

runHost :: Int -> IO ()
runHost port = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral port) 0)
  listen sock Common.backlog
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
  clientListener Server{sock, clients, clientMetadata, numClients, mailbox} loop = do
    atomically $ modifyTVar numClients succ
    clientId <- atomically $ readTVar numClients
    putStrLn "Accepting a new client"
    (clientSock, addr) <- accept sock
    conn <- socketToHandle clientSock ReadWriteMode
    hSetBuffering conn NoBuffering
    putStrLn $ "Got client: " ++ (show addr)
    let client' = Connection{clientId, conn}
    atomically $ modifyTVar clients (Map.insert clientId client')
    atomically $ modifyTVar clientMetadata (Map.insert clientId Nothing)
    _ <- forkIO $ handleClient mailbox Connection{clientId, conn}
    loop
  run server@Server{mailbox} =
    (atomically $ tryReadTQueue mailbox) >>= \case
      Just message -> handleMessage server message *> run server
      Nothing -> do
        run server

broadcast :: Server -> ByteString -> IO ()
broadcast Server{clients} message = do
  clients' <- (atomically $ readTVar clients)
  forM_ (Map.elems clients') $ \Connection{conn} -> BS.hPutStrLn conn message

handleMessage :: Server -> MessageSource -> IO ()
handleMessage server@Server{hostNick, clients} (FromHost message) =
  case message of
    Quit -> do
      broadcast server (BS.pack "Host closed room, byebye :P")
      clients' <- Map.elems <$> (atomically $ readTVar clients)
      forM_ clients' $
        \Connection{conn} -> BS.hPutStrLn conn (BS.pack "Host closed room, byebye :P") *> hClose conn
    SetNick nick -> atomically $ modifyTVar hostNick (const nick)
    ShowNick -> (atomically $ readTVar hostNick) >>= BS.putStrLn
    GotMessage msg -> do
      nick <- atomically $ readTVar hostNick
      broadcast server (BS.concat [nick, BS.pack ": ", msg])
handleMessage server@Server{clientMetadata, clients} (FromClient clientId message) =
  case message of
    Quit -> do
      metadata <- (atomically $ readTVar clientMetadata)
      msg <- case Map.lookup clientId metadata of
        Just (Just nick) -> return $ BS.concat [nick, BS.pack " quit the server"]
        Just Nothing -> return $ BS.pack $ "User " ++ (show clientId) ++ " quit the server"
        Nothing -> failIO $ "Cannot find client " ++ (show clientId) ++ "'s nick"
      atomically $ modifyTVar clients (Map.delete clientId)
      broadcast server msg
    SetNick newNick -> do
      metadata <- (atomically $ readTVar clientMetadata)
      msg <- case Map.lookup clientId metadata of
        Just (Just oldNick) ->
          return $ BS.concat [BS.pack "Client \'", oldNick, BS.pack "\' changed their nick to \'", newNick, BS.pack "\'"]
        Just Nothing -> return $ BS.concat [BS.pack $ "User " ++ (show clientId) ++ " changed their nick to ", newNick]
        Nothing -> failIO "Cannot find client nick"
      atomically $ modifyTVar clientMetadata (Map.adjust (const $ Just newNick) clientId)
      broadcast server msg
    ShowNick -> do
      metadata <- (atomically $ readTVar clientMetadata)
      nick <- case Map.lookup clientId metadata of
        Just (Just nick) -> return nick
        Just Nothing -> return $ BS.pack $ "User " ++ show clientId
        Nothing -> failIO "Cannot find client nick"
      Map.lookup clientId <$> (atomically $ readTVar clients) >>= \case
        Just Connection{conn} -> BS.hPutStrLn conn nick
        Nothing -> failIO $ "Cannot find nick for client " ++ (show clientId)
    GotMessage msg -> do
      metadata <- (atomically $ readTVar clientMetadata)
      withNick <- case Map.lookup clientId metadata of
        Just (Just nick) -> return $ BS.concat [nick, BS.pack ": ", msg]
        Just Nothing -> return msg
        Nothing -> failIO "Cannot find client nick"
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
