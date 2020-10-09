module RAIC.Model.Player where

import           GHC.Generics     (Generic)
import           RAIC.Utils.Trans (Trans)

data Player = Player {
  id    :: Int,
  score :: Int
}  deriving (Generic, Show)

instance Trans Player
