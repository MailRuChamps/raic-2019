module RAIC.MyStrategy where

import           RAIC.Debug            (Debug)
import           RAIC.Model.Game       (Game)
import           RAIC.Model.Unit       (Unit)
import           RAIC.Model.UnitAction (UnitAction)
import qualified RAIC.Model.Vec2Double as V

distanceSqr :: V.Vec2Double -> V.Vec2Double -> Double
distanceSqr a b = (V.x a - V.x b)**2 + (V.y a - V.y b)**2

getAction :: Unit -> Game -> Debug -> UnitAction
getAction unit game debug = undefined
