module RAIC.Model.UnitAction where

import           GHC.Generics          (Generic)
import           RAIC.Model.Vec2Double (Vec2Double)
import           RAIC.Utils.StreamWrapper    (Trans)

data UnitAction = UnitAction {
  velocity    :: Double,
  jump        :: Bool,
  jump_down   :: Bool,
  aim         :: Vec2Double,
  shoot       :: Bool,
  swap_weapon :: Bool,
  plant_mine  :: Bool
} deriving (Generic, Show)

instance Trans UnitAction
