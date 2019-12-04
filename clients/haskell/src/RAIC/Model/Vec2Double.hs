module RAIC.Model.Vec2Double where

import           GHC.Generics       (Generic)
import           RAIC.Utils.StreamWrapper (Trans)

data Vec2Double = Vec2Double {
  x :: Double,
  y :: Double
} deriving (Generic, Show)

instance Trans Vec2Double
