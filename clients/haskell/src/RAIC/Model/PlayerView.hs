module RAIC.Model.PlayerView where

import           GHC.Generics     (Generic)
import           RAIC.Model.Game  (Game)
import           RAIC.Utils.Trans (Trans)

data PlayerView = PlayerView {
  my_id :: Int,
  game  :: Game
} deriving (Generic, Show)

instance Trans PlayerView
