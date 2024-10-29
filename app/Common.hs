{-# LANGUAGE NamedFieldPuns #-}

module Common where

import Control.Monad (forever)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)

backlog :: Int
backlog = 10

bufSize :: Int
bufSize = 1024

class Agent a where
  connection :: a -> Socket
  disconnect :: a -> IO ()
  setNick :: a -> (String -> IO ())
  gotMessage :: a -> (String -> IO ())

listenUserIn :: (ByteString -> IO ()) -> IO ()
listenUserIn postMessage =
  forever $ do
    message <- fmap pack getLine
    postMessage message

handleMessage :: (Agent a) => a -> IO ()
handleMessage agent = do
  message <- recv (connection agent) bufSize
  go $ words $ unpack message
 where
  go [] = disconnect agent
  go ("/quit" : _) = disconnect agent
  go ("/setNick" : nick : _) = do
    setNick agent nick
    handleMessage agent
  go msg = gotMessage agent (unwords msg)
