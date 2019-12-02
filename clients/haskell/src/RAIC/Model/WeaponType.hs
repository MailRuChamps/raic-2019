module RAIC.Model.WeaponType where

import           GHC.Generics       (Generic)
import           RAIC.StreamWrapper (Trans, get, put)

data WeaponType = Pistol | AssaultRifle | RocketLauncher
  deriving (Enum, Eq, Ord, Generic, Show)

instance Trans WeaponType where
  put = put . fromEnum
  get = toEnum <$> get
