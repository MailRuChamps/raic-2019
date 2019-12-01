module RAIC.Model.ServerMessageGame where

import RAIC.Model.PlayerView (PlayerView)

newtype ServerMessageGame = ServerMessageGame {
  player_view :: Maybe PlayerView
}
