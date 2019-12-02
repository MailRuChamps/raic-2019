module RAIC.Model.PlayerView where

import           GHC.Generics       (Generic)
import           RAIC.Model.Game    (Game)
import           RAIC.StreamWrapper (Trans)

data PlayerView = PlayerView {
  my_id :: Int,
  game  :: Game
} deriving (Generic, Show)

instance Trans PlayerView
