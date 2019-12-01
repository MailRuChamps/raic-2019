module RAIC.Main where

import qualified Network.Socket            as Sock
import           RAIC.StreamWrapper        (writeString, writeBool)
import           System.Environment        (getArgs)
import           System.IO.Streams.Builder (builderStream)
import           System.IO.Streams.Network (socketToStreams)
import           System.IO.Streams.TCP     (connectSocket)

defaultHost :: String
defaultHost = "127.0.0.1"

defaultPort :: Sock.PortNumber
defaultPort = 31001

defaultToken :: String
defaultToken = "0000000000000000"

main :: IO ()
main = Sock.withSocketsDo $ do
         args <- getArgs
         let arglen = length args
         let host =
               if arglen < 1
                 then defaultHost
                 else head args
         let port = if arglen < 2
             then defaultPort
             else read (args !! 1) :: Sock.PortNumber
         let token = if arglen < 3
             then defaultToken
             else args !! 2
         (sock, _addr) <- connectSocket host port
         run token sock

-- TODO: Make the record field names consistent
-- TODO: Turn on OverloadedStrings, replace [Char] with Text
-- TODO: Consider using 'Vector' over default lists for better performance
run :: String -> Sock.Socket -> IO ()
run token sock = do
  (is, tempos) <- socketToStreams sock
  os <- builderStream tempos
  writeString token os
  writeBool True os
  Sock.close sock

---- from the "network-run" package.
--runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
--runTCPClient host port client = withSocketsDo $ do
--    addr <- resolve
--    E.bracket (open addr) close client
--  where
--    resolve = do
--        let hints = defaultHints { addrSocketType = Stream }
--        head <$> getAddrInfo (Just hints) (Just host) (Just port)
--    open addr = do
--        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--        connect sock $ addrAddress addr
--        return sock
