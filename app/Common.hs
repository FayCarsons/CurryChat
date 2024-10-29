{-# LANGUAGE NamedFieldPuns #-}

module Common where

import Control.Monad (forever)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO (Handle)

backlog :: Int
backlog = 10

bufSize :: Int
bufSize = 1024

class Agent a where
  connection :: a -> IO Handle
  disconnect :: a -> IO ()
  setNick :: a -> (String -> IO ())
  gotMessage :: a -> (String -> IO ())

listenUserIn :: (ByteString -> IO ()) -> IO ()
listenUserIn postMessage =
  forever $ do
    message <- fmap BS.pack getLine
    postMessage message

runAgent :: (Agent a) => a -> IO ()
runAgent agent = do
  message <- connection agent >>= BS.hGetLine >>= (return . BS.unpack)
  go $ words message
 where
  go [] = disconnect agent
  go ("/quit" : _) = disconnect agent
  go ("/setNick" : nick : _) = do
    setNick agent nick
    runAgent agent
  go msg = gotMessage agent (unwords msg)