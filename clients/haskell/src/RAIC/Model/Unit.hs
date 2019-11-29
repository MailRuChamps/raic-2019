module RAIC.Model.Unit where

import           RAIC.Model.JumpState  (JumpState)
import           RAIC.Model.Vec2Double (Vec2Double)
import           RAIC.Model.Weapon     (Weapon)

data Unit = Unit {
  player_id  :: Int,
  id         :: Int,
  health     :: Int,
  position   :: Vec2Double,
  size       :: Vec2Double,
  jump_state :: JumpState,
  mines      :: Int,
  weapon     :: Maybe Weapon
}
