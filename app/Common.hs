module Common where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

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

data Message
  = Quit
  | SetNick ByteString
  | ShowNick
  | GotMessage ByteString

parseMessage :: String -> Maybe Message
parseMessage message =
  if null message
    then Nothing
    else return $ case words message of
      "/quit" : _ -> Quit
      "/setNick" : nick : _ -> SetNick $ BS.pack nick
      "/showNick" : _ -> ShowNick
      msg -> GotMessage $ BS.pack (unwords msg)

backlog :: Int
backlog = 1

bufSize :: Int
bufSize = 1024
