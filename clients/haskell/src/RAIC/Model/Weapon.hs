module RAIC.Model.Weapon where

import           RAIC.Model.WeaponParams (WeaponParams)
import           RAIC.Model.WeaponType   (WeaponType)

data Weapon = Weapon {
  weapon_type :: WeaponType,
  params      :: WeaponParams,
  magazine    :: Int,
  spread      :: Double,
  file_timer  :: Maybe Double,
  last_angle  :: Maybe Double
}
