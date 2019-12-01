module RAIC.Main where

import           Control.Exception         (bracket)
import           Control.Monad             (forever, mzero, when)
import           Data.Text                 (Text, pack)
import qualified Network.Socket            as Sock
import           RAIC.StreamWrapper        (readFrom, writeTo)
import           RAIC.TCPSocket            (runTCPClient)
import           System.Environment        (getArgs)
import           System.IO.Streams.Network (socketToStreams)

defaultHost :: String
defaultHost = "127.0.0.1"

defaultPort :: Sock.ServiceName
defaultPort = "31001"

defaultToken :: String
defaultToken = "0000000000000000"

-- TODO: Consider rewriting everything with conduit and cereal-conduit
main :: IO ()
main =
  Sock.withSocketsDo $ do
    args <- getArgs
    let arglen = length args
    let host =
          if arglen < 1
            then defaultHost
            else head args
    let port =
          if arglen < 2
            then defaultPort
            else args !! 1
    let token =
          if arglen < 3
            then defaultToken
            else args !! 2
    runTCPClient host port (run (pack token))

-- TODO: Make the record field names consistent
-- TODO: Turn on OverloadedStrings, replace [Char] with Text
-- TODO: Consider using 'Vector' over default lists for better performance
run :: Text -> Sock.Socket -> IO ()
run token sock = do
  (is, os) <- socketToStreams sock
  writeTo token os
--  writeString token os
--  forever $ do
--    serverMessage <- readFrom is
--    when (IsNothing (player_view serverMessage)) mzero
  Sock.close sock
