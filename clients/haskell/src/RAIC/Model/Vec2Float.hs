module RAIC.Model.Vec2Float where

import           GHC.Generics       (Generic)
import           RAIC.Utils.StreamWrapper (Trans)

data Vec2Float = Vec2Float {
  x :: Float,
  y :: Float
} deriving (Generic, Show)

instance Trans Vec2Float
