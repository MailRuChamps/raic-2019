module RAIC.Model.CustomData where

import           Data.Binary              (Get)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           RAIC.Model.ColoredVertex (ColoredVertex)
import           RAIC.Model.ColorFloat    (ColorFloat)
import           RAIC.Model.TextAlignment (TextAlignment)
import           RAIC.Model.Vec2Float     (Vec2Float)
import           RAIC.Utils.StreamWrapper       (Trans, get, put)

data CustomData
  = Log {
    logText :: Text
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
    placedText      :: Text,
    placedPos       :: Vec2Float,
    placedAlignment :: TextAlignment,
    placedSize      :: Float,
    placedColor     :: ColorFloat
  } deriving (Generic, Show)

instance Trans CustomData where
  put (Log val) = put (0 ::Int) <> put val
  put (Rect a b c) = put (1 :: Int) <> put a <> put b <> put c
  put (Line a b c d) = put (2 :: Int) <> put a <> put b  <> put c  <> put d
  put (Polygon val) = put (3 :: Int) <> put val
  put (PlacedText a b c d e) = put (2 :: Int) <> put a <> put b  <> put c  <> put d <> put e
  get = do
    det <- get :: Get Int
    case det of
      0 -> Log <$> get
      1 -> Rect <$> get <*>  get <*>  get
      2 -> Line <$> get <*>  get <*>  get <*> get
      3 -> Polygon <$> get
      4 -> PlacedText <$> get <*>  get <*>  get <*> get <*> get
      _ -> error "Unexpected discriminant value"
