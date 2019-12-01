module RAIC.Model.Tile where

import           RAIC.StreamWrapper (Trans, get, put)

data Tile = Empty | Wall | Platform | Ladder | JumpPad
  deriving (Enum)

instance Trans Tile where
  put = put . fromEnum
  get = toEnum <$> get
