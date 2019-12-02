module RAIC.Model.WeaponParams where

import           GHC.Generics               (Generic)
import           RAIC.Model.BulletParams    (BulletParams)
import           RAIC.Model.ExplosionParams (ExplosionParams)
import           RAIC.StreamWrapper         (Trans)

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
} deriving (Generic, Show)

instance Trans WeaponParams
