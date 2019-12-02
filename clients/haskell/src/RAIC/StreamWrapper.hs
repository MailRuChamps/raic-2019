{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module RAIC.StreamWrapper where

import           Control.Exception        (throwIO)
import           Control.Monad            (replicateM)
import qualified Data.Binary              as Bin
import qualified Data.Binary.Get          as Bin.Get
import qualified Data.Binary.Put          as Bin.Put
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Int                 (Int32, Int64)
import           Data.Map.Strict          (Map, fromList, size, toList)
import           Data.Maybe               (Maybe (Just))
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import qualified GHC.Generics             as G
import qualified System.IO.Streams        as Streams
import           System.IO.Streams.Binary (DecodeException (..), getFromStream)

class Trans a where
  put :: a -> Bin.Put
  get :: Bin.Get a

  default put :: (G.Generic a, GTrans (G.Rep a)) => a -> Bin.Put
  put = gput . G.from

  default get :: (G.Generic a, GTrans (G.Rep a)) => Bin.Get a
  get = G.to `fmap` gget

-- | Generic version of 'Trans' for GHC.Generics-based deriving
class GTrans f where
  gput :: f a -> Bin.Put
  gget :: Bin.Get (f a)

-- TODO: Add generation for Enum types
-- Implementation taken from binary package
-- Type without constructors
instance GTrans G.V1 where
    gput _ = pure ()
    gget   = return undefined

-- Constructor without arguments
instance GTrans G.U1 where
    gput G.U1 = pure ()
    gget    = return G.U1

-- Product: constructor with parameters
instance (GTrans a, GTrans b) => GTrans (a G.:*: b) where
    gput (x G.:*: y) = gput x <> gput y
    gget = (G.:*:) <$> gget <*> gget

-- Metadata (constructor name, etc)
instance GTrans a => GTrans (G.M1 i c a) where
    gput = gput . G.unM1
    gget = G.M1 <$> gget

-- Constants, additional parameters, and rank-1 recursion
instance Trans a => GTrans (G.K1 i a) where
    gput = put . G.unK1
    gget = G.K1 <$> get

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

instance (Trans a, Trans b) => Trans (a, b) where
  put (x, y) = do
    put x
    put y
  get = do
    x <- get
    y <- get
    return (x, y)

instance (Trans k, Ord k, Trans v) => Trans (Map k v) where
  put val = do
    put $ size val
    put $ toList val
  get = fromList <$> get
