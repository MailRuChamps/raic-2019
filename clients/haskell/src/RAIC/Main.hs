module RAIC.Main where

import           Control.Exception.Base       (SomeException, catch)
import           Control.Monad                (forever)
import           Data.Map.Strict              (fromList)
import           Data.Text                    (Text, pack)
import qualified Network.Socket               as Sock
import           RAIC.Debug                   (Debug (Debug))
import           RAIC.Model.Game              as Game
import           RAIC.Model.PlayerMessageGame (PlayerMessageGame (ActionMessage))
import           RAIC.Model.PlayerView        (game, my_id)
import           RAIC.Model.ServerMessageGame (ServerMessageGame, player_view)
import           RAIC.Model.Unit              as Unit
import           RAIC.Model.Versioned         (Versioned (Versioned))
import           RAIC.MyStrategy              (getAction)
import           RAIC.Utils.StreamWrapper     (readFrom, writeTo)
import           RAIC.Utils.TCPSocket         (runTCPClient)
import           System.Environment           (getArgs)
import           System.IO.Streams.Network    (socketToStreams)

defaultHost :: String
defaultHost = "127.0.0.1"

defaultPort :: Sock.ServiceName
defaultPort = "31001"

defaultToken :: String
defaultToken = "0000000000000000"

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
-- TODO: Consider using 'Vector' instead of default lists []
run :: Text -> Sock.Socket -> IO ()
run token sock = do
  (is, os) <- socketToStreams sock
  writeTo token os
  catch
    (forever ((readFrom is :: IO ServerMessageGame) >>= \msg -> writeTo (runGame msg) os))
    (\e -> print (e :: SomeException) <> putStrLn "Exiting...")
  Sock.close sock

runGame :: ServerMessageGame -> PlayerMessageGame
runGame message = ActionMessage (Versioned (fromList actions))
  where
    maybePlayerView = player_view message
    playerView = case maybePlayerView of
                     Nothing  -> error "Game over"
                     (Just x) -> x
    myId = my_id playerView
    curGame = game playerView
    myUnits = filter (\x -> player_id x== myId) ((units . game) playerView)
    actions = map (\curUnit -> (Unit.id curUnit, getAction curUnit curGame Debug)) myUnits
