module RAIC.Model.ColoredVertex where

import           RAIC.Model.ColorFloat (ColorFloat)
import           RAIC.Model.Vec2Float  (Vec2Float)

data ColoredVertex = ColoredVertex {
  position :: Vec2Float,
  color    :: ColorFloat
}
