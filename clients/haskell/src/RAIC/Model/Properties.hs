module RAIC.Model.Properties where

import           RAIC.Model.ExplosionParams (ExplosionParams)
import           RAIC.Model.Vec2Double      (Vec2Double)
import           RAIC.Model.WeaponParams    (WeaponParams)
import           RAIC.Model.WeaponType      (WeaponType)

data Properties = Properties {
  max_tick_count            :: Int,
  ticks_per_second          :: Double,
  updates_per_tick          :: Int,
  loot_box_sie              :: Vec2Double,
  unit_size                 :: Vec2Double,
  unit_max_horizontal_speed :: Double,
  unit_fall_speed           :: Double,
  unit_jump_time            :: Double,
  unit_jump_speed           :: Double,
  jump_pad_jump_time        :: Double,
  jump_pad_jump_speed       :: Double,
  unit_max_health           :: Int,
  -- TODO: Consider using Data.Map.Strict from containers
  weapon_params             :: [(WeaponType, WeaponParams)],
  mine_size                 :: Vec2Double,
  mine_explosion_params     :: ExplosionParams,
  mine_prepare_time         :: Double,
  mine_trigger_time         :: Double,
  mine_trigger_radius       :: Double,
  kill_score                :: Int
}
