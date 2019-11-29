module RAIC.Model.CustomData where

import           RAIC.Model.ColoredVertex (ColoredVertex)
import           RAIC.Model.ColorFloat    (ColorFloat)
import           RAIC.Model.TextAlignment (TextAlignment)
import           RAIC.Model.Vec2Float     (Vec2Float)

data CustomData
  = Log {
    logText :: String
  }
  | Rect {
    rectPos   :: Vec2Float,
    rectSize  :: Vec2Float,
    rectColor :: ColorFloat
  }
  | Line {
    lineP1    :: Vec2Float,
    lineP2    :: Vec2Float,
    lineWidth :: Float,
    lineColor :: ColorFloat
  }
  | Polygon {
    polyVertices :: [ColoredVertex]
  }
  | PlacedText {
    placedText      :: String,
    placedPos       :: Vec2Float,
    placedAlignment :: TextAlignment,
    placedSize      :: Float,
    placedColor     :: ColorFloat
  }
