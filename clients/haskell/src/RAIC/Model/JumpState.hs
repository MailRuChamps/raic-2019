module RAIC.Model.JumpState where

data JumpState = JumpState {
  can_jump   :: Bool,
  speed      :: Double,
  max_time   :: Double,
  can_cancel :: Bool
}
