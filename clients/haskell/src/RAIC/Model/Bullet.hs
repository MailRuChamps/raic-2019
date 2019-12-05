module RAIC.Model.Bullet (Bullet) where

import           GHC.Generics               (Generic)
import           RAIC.Model.ExplosionParams (ExplosionParams)
import           RAIC.Model.Vec2Double      (Vec2Double)
import           RAIC.Model.WeaponType      (WeaponType)
import           RAIC.Utils.Trans           (Trans)

data Bullet = Bullet {
  weapon_type      :: WeaponType,
  unit_id          :: Int,
  player_id        :: Int,
  position         :: Vec2Double,
  velocity         :: Vec2Double,
  damage           :: Int,
  size             :: Double,
  explosion_params :: Maybe ExplosionParams
} deriving (Generic, Show)

instance Trans Bullet
