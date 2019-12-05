module RAIC.Model.ColoredVertex where

import           GHC.Generics          (Generic)
import           RAIC.Model.ColorFloat (ColorFloat)
import           RAIC.Model.Vec2Float  (Vec2Float)
import           RAIC.Utils.Trans      (Trans)

data ColoredVertex = ColoredVertex {
  position :: Vec2Float,
  color    :: ColorFloat
} deriving (Generic, Show)

instance Trans ColoredVertex
