{-# LANGUAGE ScopedTypeVariables #-}
module RAIC.Debug where

import qualified Data.Binary                  as Bin
import qualified Data.Binary.Get              as Bin.Get
import qualified Data.Binary.Put              as Bin.Put
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import           Data.Map.Strict              (Map)
import           RAIC.Model.Game              (Game)
import           RAIC.Model.Properties        (Properties)
import           RAIC.Model.ServerMessageGame (ServerMessageGame)
import           RAIC.Model.Vec2Double        (Vec2Double)
import           RAIC.Model.WeaponParams      (WeaponParams)
import           RAIC.Model.WeaponType        (WeaponType)
import           RAIC.Utils.Trans             (Trans, get, put)

data Debug = Debug

--draw :: CustomData -> BinaryWriter ()
--draw = putStrLn

encode :: Trans a => a -> [Bin.Word8]
encode x = BL.unpack (Bin.Put.runPut (put x))

parse :: Bin.Get a -> B.ByteString -> a
parse parser str = Bin.Get.runGet parser (BL.fromStrict str)

-- Map WeaponType WeaponParams
check :: Trans a => Int -> IO a
check offset = do
  raw <- B.readFile "serverMessage.bin"
  let str = B.drop offset raw
  return $ parse get str
