module RAIC.Model.BulletParams where

import           GHC.Generics       (Generic)
import           RAIC.Utils.StreamWrapper (Trans)

data BulletParams = BulletParams {
  speed  :: Double,
  size   :: Double,
  damage :: Int
} deriving (Generic, Show)

instance Trans BulletParams
