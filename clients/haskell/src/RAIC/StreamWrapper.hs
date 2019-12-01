module RAIC.StreamWrapper where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as Build
import           Data.ByteString.Builder.Extra (flush)
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.UTF8     as UTF8
import           Data.Int                      (Int32, Int64)
import qualified System.IO.Streams             as Streams
import qualified Data.Binary.Get as Bin

--class Trans a where
--  read_from :: Streams.InputStream Str.ByteString -> IO (Maybe a)
--  write_to :: a -> Streams.OutputStream Build.Builder -> IO ()
--instance Trans Int - Int32
--instance Trans Integer - Int64

readBool :: Streams.InputStream B.ByteString-> IO Bool
readBool is = do
  rawByte <- Streams.readExactly 1 is
  let word8 = Bin.runGet Bin.getWord8 (BL.fromStrict rawByte)
  return (word8 /= 0)

readInt :: Streams.InputStream B.ByteString -> IO Int
readInt is = do
  rawBytes <- Streams.readExactly 4 is
  let word32 = Bin.runGet Bin.getWord32le (BL.fromStrict rawBytes)
  return (fromIntegral word32 :: Int)

readLong :: Streams.InputStream B.ByteString -> IO Integer
readLong is = do
  rawBytes <- Streams.readExactly 8 is
  let word64 = Bin.runGet Bin.getWord64le (BL.fromStrict rawBytes)
  return (fromIntegral word64 :: Integer)

readFloat :: Streams.InputStream B.ByteString -> IO Float
readFloat is = do
  rawBytes <- Streams.readExactly 4 is
  return $ Bin.runGet Bin.getFloatle (BL.fromStrict rawBytes)

readDouble :: Streams.InputStream B.ByteString -> IO Double
readDouble is = do
  rawBytes <- Streams.readExactly 8 is
  return $ Bin.runGet Bin.getDoublele (BL.fromStrict rawBytes)

readString :: Streams.InputStream B.ByteString -> IO String
readString is = do
  len <- readInt is
  rawBytes <- Streams.readExactly len is
  return $ UTF8.toString (BL.fromStrict rawBytes)

writeBool :: Bool -> Streams.OutputStream Build.Builder -> IO ()
writeBool x out = do
  Streams.write (Just (Build.word8 (if x then 1 else 0))) out
  Streams.write (Just flush) out

writeInt :: Int -> Streams.OutputStream Build.Builder -> IO ()
writeInt x out = do
  Streams.write  (Just (Build.int32LE (fromIntegral x :: Int32))) out
  Streams.write (Just flush) out

writeLong :: Int -> Streams.OutputStream Build.Builder -> IO ()
writeLong x out = do
  Streams.write  (Just (Build.int64LE (fromIntegral x :: Int64))) out
  Streams.write (Just flush) out

writeFloat :: Float -> Streams.OutputStream Build.Builder -> IO ()
writeFloat x out = do
  Streams.write  (Just (Build.floatLE x)) out
  Streams.write (Just flush) out

writeDouble :: Double -> Streams.OutputStream Build.Builder -> IO ()
writeDouble x out = do
  Streams.write  (Just (Build.doubleLE x)) out
  Streams.write (Just flush) out

writeString :: String -> Streams.OutputStream Build.Builder -> IO ()
writeString text out = do
  let bintext = UTF8.fromString text
  let len = BL.length bintext
  writeInt (fromIntegral len :: Int) out
  Streams.write (Just (Build.lazyByteString bintext)) out
  Streams.write (Just flush) out
