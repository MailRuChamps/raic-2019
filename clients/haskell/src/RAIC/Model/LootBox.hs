module RAIC.Model.LootBox where

import           GHC.Generics          (Generic)
import           RAIC.Model.Item       (Item)
import           RAIC.Model.Vec2Double (Vec2Double)
import           RAIC.StreamWrapper    (Trans)

data LootBox = LootBox {
  position :: Vec2Double,
  size     :: Vec2Double,
  item     :: Item
} deriving (Generic, Show)

instance Trans LootBox
