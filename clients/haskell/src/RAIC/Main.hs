{-# LANGUAGE OverloadedStrings #-}
module RAIC.Main where

import qualified Control.Exception         as E
import qualified Data.ByteString.UTF8     as C
import           Network.Socket            hiding (defaultPort, recv)
import           Network.Socket.ByteString (recv, sendAll)
import           System.Environment        (getArgs)

defaultHost :: String
defaultHost = "127.0.0.1"

defaultPort :: String
defaultPort = "31001"

defaultToken :: String
defaultToken = "0000000000000000"

-- TODO: Make the record field names consistent
-- TODO: Turn on OverloadedStrings, replace [Char] with Text
-- TODO: Consider using 'Vector' over default lists for better performance
run :: IO ()
run = do
  args <- getArgs
  let arglen = length args
  let host =
        if arglen < 1
          then defaultHost
          else head args
  let port = if arglen < 2
      then defaultPort
      else args !! 1
  let token = C.fromString (if arglen < 3
      then defaultToken
      else args !! 2)
  let tokenlen = C.length token
  runTCPClient host port $ \sock -> do
    sendAll sock (C.fromString (show tokenlen))
    sendAll sock token
    msg <- recv sock 1024
    putStr "Received: "
    putStrLn (C.toString msg)


-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
