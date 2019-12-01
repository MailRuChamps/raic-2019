{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module RAIC.StreamWrapper where

import           Control.Exception         (throwIO)
import qualified Data.Binary               as Bin
import qualified Data.Binary.Get           as Bin.Get
import qualified Data.Binary.Put           as Bin.Put
import           Data.Binary.Get.Internal  (getByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.Int                  (Int32, Int64)
import qualified System.IO.Streams         as Streams
import           System.IO.Streams.Binary  (DecodeException (..), getFromStream,
                                            putToStream, decodeFromStream)

class Trans a where
  readFrom :: Streams.InputStream B.ByteString -> IO (Maybe a)
  writeTo :: a -> Streams.OutputStream B.ByteString -> IO ()
  readFromUnsafe :: Streams.InputStream B.ByteString -> IO a
  readFromUnsafe is = do
    res <- readFrom is
    case res of
      Nothing -> throwIO $ DecodeException "" 0 "End Of File exception"
      (Just value) -> return value

instance Trans Bool where
  readFrom = getFromStream (Bin.get :: Bin.Get Bool)
  writeTo x = putToStream (Just x)


newtype Int32le = Int32le { int32le :: Int32 }
instance Bin.Binary Int32le where
  get = fmap Int32le Bin.Get.getInt32le
  put x = Bin.Put.putInt32le (int32le x)

instance Trans Int where
  readFrom is = fmap (fromIntegral :: Int32 -> Int) <$> decodeFromStream is :: Int32le
  writeTo x = putToStream (Just (fromIntegral x :: Int32))

instance Trans Integer where
  readFrom is = fmap (fromIntegral :: Int64 -> Integer) <$> getFromStream (Bin.get :: Bin.Get Int64) is
  writeTo x = putToStream (Just (fromIntegral x :: Int64))

instance Trans Float where
  readFrom = getFromStream (Bin.get :: Bin.Get Float)
  writeTo x = putToStream (Just x)

instance Trans Double where
  readFrom = getFromStream (Bin.get :: Bin.Get Double)
  writeTo x = putToStream (Just x)

-- Wrap type around the default implementation from Binary
newtype MyBinary = MyBinary { myBinary :: B.ByteString }

instance Bin.Binary MyBinary where
  get = fmap MyBinary getBString
  put x = putBString (myBinary x)

getBString :: Bin.Get B.ByteString
getBString = Bin.Get.getInt32le >>= \len -> getByteString (fromIntegral len)

putBString :: B.ByteString -> Bin.Put
putBString bs = Bin.Put.putInt32le (fromIntegral (B.length bs):: Int32) <> Bin.Put.putByteString bs

instance Trans String where
  readFrom is = do
    raw <- getFromStream (Bin.get :: Bin.Get MyBinary) is
    return (fmap (UTF8.toString . BL.fromStrict . myBinary) raw)
  writeTo x os = do
    let raw = MyBinary ((BL.toStrict . UTF8.fromString) x)
    putToStream (Just raw) os
