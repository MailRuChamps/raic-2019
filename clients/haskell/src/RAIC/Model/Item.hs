module RAIC.Model.Item where

import           GHC.Generics       (Generic)
import           RAIC.Utils.StreamWrapper (Trans, get, put)

data Item = HealthPack | Weapon | Mine
  deriving (Enum, Generic, Show)

instance Trans Item where
  put = put . fromEnum
  get = toEnum <$> get
