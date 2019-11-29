module RAIC.Model.WeaponParams where

import           RAIC.Model.BulletParams    (BulletParams)
import           RAIC.Model.ExplosionParams (ExplosionParams)

data WeaponParams = WeaponParams {
  magazine_size :: Int,
  fire_rate     :: Double,
  reload_time   :: Double,
  min_spread    :: Double,
  max_spread    :: Double,
  recoil        :: Double,
  aim_speed     :: Double,
  bullet        :: BulletParams,
  explosion     :: Maybe ExplosionParams
}
