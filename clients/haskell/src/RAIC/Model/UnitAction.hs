module RAIC.Model.UnitAction where

import           RAIC.Model.Vec2Double (Vec2Double)

data UnitAction = UnitAction {
  velocity    :: Double,
  jump        :: Bool,
  jump_down   :: Bool,
  aim         :: Vec2Double,
  shoot       :: Bool,
  swap_weapon :: Bool,
  plant_mine  :: Bool
}
