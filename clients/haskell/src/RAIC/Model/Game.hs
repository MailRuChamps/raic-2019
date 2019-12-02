module RAIC.Model.Game where

import           GHC.Generics          (Generic)
import           RAIC.Model.Bullet     (Bullet)
import           RAIC.Model.Level      (Level)
import           RAIC.Model.LootBox    (LootBox)
import           RAIC.Model.Mine       (Mine)
import           RAIC.Model.Player     (Player)
import           RAIC.Model.Properties (Properties)
import           RAIC.Model.Unit       (Unit)
import           RAIC.StreamWrapper    (Trans)

data Game = Game {
  current_tick :: Int,
  properties   :: Properties,
  level        :: Level,
  players      :: [Player],
  units        :: [Unit],
  bullets      :: [Bullet],
  mines        :: [Mine],
  loot_boxes   :: [LootBox]
} deriving (Generic, Show)

instance Trans Game
