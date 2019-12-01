module RAIC.Model.TextAlignment where

import RAIC.StreamWrapper (Trans, put, get)

data TextAlignment = Left | Center | Right
  deriving (Enum)

instance Trans TextAlignment where
  put = put . fromEnum
  get = toEnum <$> get
