module RAIC.Model.Properties where

import           Data.Map.Strict            (Map)
import           GHC.Generics               (Generic)
import           RAIC.Model.ExplosionParams (ExplosionParams)
import           RAIC.Model.Vec2Double      (Vec2Double)
import           RAIC.Model.WeaponParams    (WeaponParams)
import           RAIC.Model.WeaponType      (WeaponType)
import           RAIC.StreamWrapper         (Trans)

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
  weapon_params             :: Map WeaponType WeaponParams,
  mine_size                 :: Vec2Double,
  mine_explosion_params     :: ExplosionParams,
  mine_prepare_time         :: Double,
  mine_trigger_time         :: Double,
  mine_trigger_radius       :: Double,
  kill_score                :: Int
} deriving (Generic, Show)

instance Trans Properties
