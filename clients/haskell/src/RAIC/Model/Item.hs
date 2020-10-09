module RAIC.Model.Item where

import           Data.Binary.Get       (Get)
import           GHC.Generics          (Generic)
import           RAIC.Model.WeaponType (WeaponType)
import           RAIC.Utils.Trans      (Trans, get, put)

data Item
  = HealthPack { health :: Int }
  | Weapon { weapon_type :: WeaponType }
  | Mine
  deriving (Generic, Show)

instance Trans Item where
  put (HealthPack val) = put (0 :: Int) <> put val
  put (Weapon val)     = put (1 :: Int) <> put val
  put Mine             = put (2 ::Int)
  get = do
    tag <- get :: Get Int
    case tag of
          0 -> HealthPack <$> get
          1 -> Weapon <$> get
          2 -> return Mine
          _ -> error "Unexpected discriminant value"
