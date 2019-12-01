module RAIC.Model.Vec2Double where

import           RAIC.StreamWrapper (Trans, get, put)

data Vec2Double = Vec2Double {
  x :: Double,
  y :: Double
}

instance Trans Vec2Double where
  put val = do
    put (x val)
    put (y val)
  get = Vec2Double <$> get <*> get
