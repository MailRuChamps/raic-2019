module RAIC.Model.JumpState where

import           GHC.Generics       (Generic)
import           RAIC.StreamWrapper (Trans)

data JumpState = JumpState {
  can_jump   :: Bool,
  speed      :: Double,
  max_time   :: Double,
  can_cancel :: Bool
} deriving (Generic, Show)

instance Trans JumpState
