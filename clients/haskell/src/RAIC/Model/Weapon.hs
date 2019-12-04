module RAIC.Model.Weapon where

import           GHC.Generics            (Generic)
import           RAIC.Model.WeaponParams (WeaponParams)
import           RAIC.Model.WeaponType   (WeaponType)
import           RAIC.Utils.StreamWrapper      (Trans)

data Weapon = Weapon {
  weapon_type :: WeaponType,
  params      :: WeaponParams,
  magazine    :: Int,
  spread      :: Double,
  file_timer  :: Maybe Double,
  last_angle  :: Maybe Double
} deriving (Generic, Show)

instance Trans Weapon
