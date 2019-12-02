module RAIC.Model.ServerMessageGame where

import           GHC.Generics          (Generic)
import           RAIC.Model.PlayerView (PlayerView)
import           RAIC.StreamWrapper    (Trans)

newtype ServerMessageGame = ServerMessageGame {
  player_view :: Maybe PlayerView
} deriving (Generic, Show)

instance Trans ServerMessageGame
