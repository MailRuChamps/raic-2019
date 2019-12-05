module RAIC.Utils.StreamWrapper where

import           Control.Exception        (throwIO)
import qualified Data.Binary.Put          as Bin.Put
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           RAIC.Utils.Trans         (Trans, get, put)
import qualified System.IO.Streams        as Streams
import           System.IO.Streams.Binary (DecodeException (..), getFromStream)

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
