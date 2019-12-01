module RAIC.Model.Level where

import           RAIC.Model.Tile (Tile)
import           RAIC.StreamWrapper (Trans, get, put)
import Control.Monad (replicateM)

-- TODO: Consider using typed length vectors instead [Tile]
newtype Level = Level {
  tiles :: [[Tile]]
}

instance Trans Level where
  put val = do
    put $ length (tiles val)
    mconcat $ map put (tiles val)
  get = do
    height <- get
    Level <$> replicateM height get
