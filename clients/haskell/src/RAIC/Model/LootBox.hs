module RAIC.Model.LootBox where

import           RAIC.Model.Item       (Item)
import           RAIC.Model.Vec2Double (Vec2Double)
import RAIC.StreamWrapper (Trans, put, get)

data LootBox = LootBox {
  position :: Vec2Double,
  size     :: Vec2Double,
  item     :: Item
}

instance Trans LootBox where
  put val = do
    put (position val)
    put (size val)
    put (item val)
  get = LootBox <$> get <*> get <*> get
