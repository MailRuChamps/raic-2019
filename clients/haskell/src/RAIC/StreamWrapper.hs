{-# LANGUAGE FlexibleInstances #-}
module RAIC.StreamWrapper where

import           Control.Exception         (throwIO)
import qualified Data.Binary               as Bin
import qualified Data.Binary.Get           as Bin.Get
import qualified Data.Binary.Put           as Bin.Put
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import           Data.Int                  (Int32, Int64)
import           Data.Maybe                (Maybe (Just))
import qualified System.IO.Streams         as Streams
import           System.IO.Streams.Binary  (DecodeException (..), getFromStream)
import Control.Monad (replicateM)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

class Trans a where
  put :: a -> Bin.Put
  get :: Bin.Get a

-- | Write serializable data to stream
writeTo :: Trans a => a -> Streams.OutputStream B.ByteString -> IO ()
writeTo x = Streams.write (Just (BL.toStrict (Bin.Put.runPut (put x))))

-- | Read serializable data from stream
readFrom :: Trans a => Streams.InputStream B.ByteString -> IO a
readFrom is = do
  res <- getFromStream get is
  case res of
    Nothing      -> throwIO $ DecodeException "" 0 "End Of File exception"
    (Just value) -> return value

instance Trans Bool where
  put = Bin.put
  get = Bin.get

instance Trans Int where
  put = Bin.Put.putInt32le . fromIntegral
  get = (fromIntegral :: Int32 -> Int) <$> Bin.Get.getInt32le

instance Trans Integer where
  put = Bin.Put.putInt64le . fromIntegral
  get = (fromIntegral :: Int64 -> Integer) <$> Bin.Get.getInt64le

instance Trans Float where
  put = Bin.Put.putFloatle
  get = Bin.Get.getFloatle

instance Trans Double where
  put = Bin.Put.putDoublele
  get = Bin.Get.getDoublele

instance Trans Text where
  put line = do
    let raw = encodeUtf8 line
    put . (fromIntegral :: Int64 -> Int) . BL.length . BL.fromStrict $ raw
    Bin.Put.putByteString raw
  get = do
    len <- get
    decodeUtf8 <$> Bin.Get.getByteString len

instance (Trans a) => Trans [a] where
  put val = do
    put (length val)
    mconcat (map put val)
  get = do
    len <- get :: Bin.Get Int
    replicateM len get

instance Trans a => Trans (Maybe a) where
  put Nothing  = put False
  put (Just x) = put x
  get = do
    is_some <- get :: Bin.Get Bool
    if is_some
      then Just <$> get
      else return Nothing
