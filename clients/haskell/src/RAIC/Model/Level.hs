module RAIC.Model.Level where

import           GHC.Generics       (Generic)
import           RAIC.Model.Tile    (Tile)
import           RAIC.StreamWrapper (Trans)

-- TODO: Consider using typed length vectors instead [Tile]
newtype Level = Level {
  tiles :: [[Tile]]
} deriving (Generic, Show)

instance Trans Level
