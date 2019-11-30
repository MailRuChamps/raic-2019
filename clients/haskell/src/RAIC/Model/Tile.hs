module RAIC.Model.Tile where

data Tile = Empty | Wall | Platform | Ladder | JumpPad
  deriving (Enum)
