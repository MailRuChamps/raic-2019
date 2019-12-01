module RAIC.Model.Item where

import RAIC.StreamWrapper (Trans, put, get)

data Item = HealthPack | Weapon | Mine
  deriving (Enum)

instance Trans Item where
  put = put . fromEnum
  get = toEnum <$> get
