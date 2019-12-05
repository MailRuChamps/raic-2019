module RAIC.Model.ExplosionParams where

import           GHC.Generics     (Generic)
import           RAIC.Utils.Trans (Trans)

data ExplosionParams = ExplosionParams {
  radius :: Double,
  damage :: Int
} deriving (Generic, Show)

instance Trans ExplosionParams
