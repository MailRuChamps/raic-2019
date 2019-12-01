module RAIC.Model.ExplosionParams where

import           RAIC.StreamWrapper (Trans, get, put)

data ExplosionParams = ExplosionParams {
  radius :: Double,
  damage :: Int
}

instance Trans ExplosionParams where
  put val = do
    put . radius $ val
    put . damage $ val
  get = ExplosionParams <$> get <*> get
