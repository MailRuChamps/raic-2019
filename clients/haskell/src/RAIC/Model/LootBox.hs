module RAIC.Model.LootBox where

import           RAIC.Model.Item       (Item)
import           RAIC.Model.Vec2Double (Vec2Double)

data LootBox = LootBox {
  position :: Vec2Double,
  size     :: Vec2Double,
  item     :: Item
}
