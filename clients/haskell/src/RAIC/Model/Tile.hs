module RAIC.Model.Tile where

import           GHC.Generics       (Generic)
import           RAIC.StreamWrapper (Trans, get, put)

data Tile = Empty | Wall | Platform | Ladder | JumpPad
  deriving (Enum, Generic, Show)

instance Trans Tile where
  put = put . fromEnum
  get = toEnum <$> get
