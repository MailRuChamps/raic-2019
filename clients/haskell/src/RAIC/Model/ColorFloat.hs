module RAIC.Model.ColorFloat where

import           GHC.Generics       (Generic)
import           RAIC.Utils.StreamWrapper (Trans)

data ColorFloat = ColorFloat {
  r :: Float,
  g :: Float,
  b :: Float,
  a :: Float
} deriving (Generic, Show)

instance Trans ColorFloat
