module RAIC.Model.MineState where
import           GHC.Generics     (Generic)
import           RAIC.Utils.Trans (Trans, get, put)

data MineState = Preparing | Idle | Triggered | Exploded
  deriving (Enum, Generic, Show)

instance Trans MineState where
  put = put . fromEnum
  get = toEnum <$> get
