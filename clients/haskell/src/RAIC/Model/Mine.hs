module RAIC.Model.Mine where

import           RAIC.Model.ExplosionParams (ExplosionParams)
import           RAIC.Model.MineState
import           RAIC.Model.Vec2Double      (Vec2Double)

data Mine = Mine {
  player_id        :: Int,
  position         :: Vec2Double,
  size             :: Vec2Double,
  state            :: MineState,
  timer            :: Maybe Double,
  trigger_radius   :: Double,
  explosion_params :: ExplosionParams
}
