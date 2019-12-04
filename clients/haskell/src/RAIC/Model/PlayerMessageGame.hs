module RAIC.Model.PlayerMessageGame where

import           Data.Binary           (Get)
import           Data.Map.Strict       (Map)
import           RAIC.Model.CustomData (CustomData)
import           RAIC.Model.UnitAction (UnitAction)
import           RAIC.Utils.StreamWrapper    (Trans, get, put)

data PlayerMessageGame =
  CustomData {
    _data :: CustomData
  }
  | ActionMessage {
    action :: Map Int UnitAction
  } deriving (Show)

instance Trans PlayerMessageGame where
  put (CustomData val)    = put (0 ::Int) <> put val
  put (ActionMessage val) = put (1 :: Int) <> put val
  get = do
    det <- get :: Get Int
    case det of
      0 -> CustomData <$> get
      1 -> ActionMessage <$> get
      _ -> error "Unexpected discriminant value"
