module RAIC.Model.MineState where
import           GHC.Generics       (Generic)
import           RAIC.StreamWrapper (Trans, get, put)

data MineState = Preparing | Idle | Triggered
  deriving (Enum, Generic, Show)

instance Trans MineState where
  put = put . fromEnum
  get = toEnum <$> get
