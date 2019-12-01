module RAIC.Model.Bullet (Bullet) where

import           RAIC.Model.ExplosionParams (ExplosionParams)
import           RAIC.Model.Vec2Double      (Vec2Double)
import           RAIC.Model.WeaponType      (WeaponType)

data Bullet = Bullet {
  weapon_type      :: WeaponType,
  unit_id          :: Int,
  player_id        :: Int,
  position         :: Vec2Double,
  velocity         :: Vec2Double,
  damage           :: Int,
  size             :: Double,
  explosion_params :: Maybe ExplosionParams
}

--writeBullet :: Bullet -> OutputStream Builder -> IO ()
--writeBullet bullet out = do
--  writeWeaponType (weapon_type bullet) out
--  writeInt (unit_id bullet) out
--  