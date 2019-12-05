module RAIC.Model.Weapon where

import           GHC.Generics            (Generic)
import           RAIC.Model.WeaponParams (WeaponParams)
import           RAIC.Model.WeaponType   (WeaponType)
import           RAIC.Utils.Trans        (Trans)

data Weapon = Weapon {
  weapon_type    :: WeaponType,
  params         :: WeaponParams,
  magazine       :: Int,
  was_shooting   :: Bool,
  spread         :: Double,
  file_timer     :: Maybe Double,
  last_angle     :: Maybe Double,
  last_fire_tick :: Maybe Int
} deriving (Generic, Show)

instance Trans Weapon
