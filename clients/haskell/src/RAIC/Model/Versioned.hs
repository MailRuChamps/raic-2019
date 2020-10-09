module RAIC.Model.Versioned where

import           Data.Map.Strict       (Map)
import           GHC.Generics          (Generic)
import           RAIC.Model.UnitAction (UnitAction)
import           RAIC.Utils.Trans      (Trans, get, put)

magic :: Int
magic = 43981

newtype Versioned = Versioned {
  inner :: Map Int UnitAction
} deriving (Generic, Show)

instance Trans Versioned where
  put val = put magic <> put (inner val)
  get = Versioned <$> get
