module RAIC.Model.PlayerView where

import RAIC.Model.Game (Game)

data PlayerView = PlayerView {
  my_id :: Int,
  game :: Game
}
