module RAIC.Model.TextAlignment where

import           GHC.Generics     (Generic)
import           RAIC.Utils.Trans (Trans, get, put)

data TextAlignment = Left | Center | Right
  deriving (Enum, Generic, Show)

instance Trans TextAlignment where
  put = put . fromEnum
  get = toEnum <$> get
