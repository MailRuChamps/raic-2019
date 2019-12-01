module RAIC.Model.Vec2Float where

import           RAIC.StreamWrapper (Trans, get, put)

data Vec2Float = Vec2Float {
  x :: Float,
  y :: Float
}

instance Trans Vec2Float where
  put val = do
    put (x val)
    put (y val)
  get = Vec2Float <$> get <*> get
