module RAIC.Model.WeaponType where

import           RAIC.StreamWrapper (Trans, get, put)

data WeaponType = Pistol | AssaultRifle | RocketLauncher
  deriving (Enum)

instance Trans WeaponType where
  put = put . fromEnum
  get = toEnum <$> get
