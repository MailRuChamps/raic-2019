module RAIC.Model.Player where

import           GHC.Generics       (Generic)
import           RAIC.StreamWrapper (Trans)

data Player = Player {
  id    :: Int,
  score :: Int
}  deriving (Generic, Show)

instance Trans Player
