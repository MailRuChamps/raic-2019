module RAIC.Model.PlayerMessageGame where

import           Data.Binary           (Get)
import           RAIC.Model.CustomData (CustomData)
import           RAIC.Model.Versioned  (Versioned)
import           RAIC.Utils.Trans      (Trans, get, put)

data PlayerMessageGame =
  CustomData {
    _data :: CustomData
  }
  | ActionMessage {
    action :: Versioned
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
