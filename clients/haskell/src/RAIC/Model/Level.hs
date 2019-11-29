module RAIC.Model.Level where

import           RAIC.Model.Tile (Tile)

-- TODO: Consider using typed length vectors instead [Tile]
newtype Level = Level{tiles :: [[Tile]]}
