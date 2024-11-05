module Common where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

appHeader :: String
appHeader =
  unlines
    [ "                                                "
    , "   ██████╗██╗   ██╗██████╗ ██████╗ ██╗   ██╗   "
    , "  ██╔════╝██║   ██║██╔══██╗██╔══██╗╚██╗ ██╔╝   "
    , "  ██║     ██║   ██║██████╔╝██████╔╝ ╚████╔╝    "
    , "  ██║     ██║   ██║██╔══██╗██╔══██╗  ╚██╔╝     "
    , "  ╚██████╗╚██████╔╝██║  ██║██║  ██║   ██║      "
    , "   ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝      "
    , "                                                "
    , "     ██████╗██╗  ██╗ █████╗ ████████╗          "
    , "    ██╔════╝██║  ██║██╔══██╗╚══██╔══╝          "
    , "    ██║     ███████║███████║   ██║             "
    , "    ██║     ██╔══██║██╔══██║   ██║             "
    , "    ╚██████╗██║  ██║██║  ██║   ██║             "
    , "     ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝             "
    , "                                                "
    ]

clearScreen :: IO ()
clearScreen =
  putStr "\ESC[2J"
    *> putStr "\ESC[H"
    *> hFlush stdout

data Message
  = Quit
  | SetNick ByteString
  | ShowNick
  | GotMessage ByteString

parseMessage :: String -> Maybe Message
parseMessage message =
  if null message
    then Just Quit
    else return $ case words message of
      "/quit" : _ -> Quit
      "/setNick" : nick -> SetNick $ pack nick
      "/showNick" : _ -> ShowNick
      msg -> GotMessage $ pack msg
 where
  pack = BS.unwords . map BS.pack

backlog :: Int
backlog = 1

bufSize :: Int
bufSize = 1024
