module RAIC.Model.TextAlignment where

import           GHC.Generics       (Generic)
import           RAIC.StreamWrapper (Trans, get, put)

data TextAlignment = Left | Center | Right
  deriving (Enum, Generic, Show)

instance Trans TextAlignment where
  put = put . fromEnum
  get = toEnum <$> get
