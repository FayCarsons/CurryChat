{-# LANGUAGE NamedFieldPuns #-}

module Common where

import Control.Concurrent (yield)
import Control.Exception (Exception, SomeException (SomeException), bracket, displayException, handle)
import Control.Monad (forever, when)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO (Handle, hClose, hFlush, hIsReadable)

backlog :: Int
backlog = 10

bufSize :: Int
bufSize = 1024

data Flow
  = Continue
  | Quit

class Agent a where
  getName :: a -> String
  connection :: a -> Handle
  disconnect :: a -> IO ()
  showNick :: a -> IO ()
  setNick :: a -> (ByteString -> IO ())
  gotMessage :: a -> (ByteString -> IO ())

atomicWrite :: (Agent a) => a -> ByteString -> IO Flow
atomicWrite agent msg = do
  putStrLn $ "Client writing message " ++ (BS.unpack msg)
  bracket
    (return $ connection agent)
    (hClose)
    ( \h ->
        handle (handleError agent) (BS.hPut h msg *> return Continue)
    )

runAgent :: (Agent a) => a -> IO Flow
runAgent agent = handle (handleError agent) $ do
  let conn = (connection agent)
  ready <- hIsReadable conn
  if ready
    then do
      message <- BS.hGetNonBlocking conn bufSize
      case BS.words message of
        [] -> return Continue
        [word] -> do
          putStrLn $ (getName agent) ++ " got a message!"
          gotMessage agent word
          return Continue
        cmd : next -> do
          putStrLn $ (getName agent) ++ " got a message!"
          case BS.unpack cmd of
            "/quit" -> disconnect agent *> return Quit
            "/setNick" -> setNick agent (BS.unwords next) *> return Continue
            "/showNick" -> showNick agent *> return Continue
            _ -> gotMessage agent message *> return Continue
    else yield *> return Continue

handleError :: (Agent a) => a -> IOError -> IO Flow
handleError agent e = do
  putStrLn $
    getName agent ++ " failed: " ++ displayException e
  disconnect agent
  return Quit
