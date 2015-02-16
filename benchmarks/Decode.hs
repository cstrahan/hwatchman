{-# LANGUAGE ImpredicativeTypes #-}

import           Control.Applicative
import           Control.Monad
import           Criterion.Main
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import           Data.IORef
import           Foreign.ForeignPtr
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           System.IO
import           System.Watchman

-- just to help with inference
zero :: Int
zero = 0

-- make a stream reader from a fixture file.
makeReader :: BS.ByteString -> IO (Ptr a -> Int -> IO Int)
makeReader str = do
    posRef <- newIORef 0
    let (fptr, offset, size) = BS.toForeignPtr str
    return $ \destPtr len -> do
        withForeignPtr fptr $ \ptr -> do
            pos <- readIORef posRef
            let srcPtr = plusPtr ptr (offset + pos)
            copyBytes destPtr srcPtr len
            writeIORef posRef (pos + len)
            return len

main :: IO ()
main = do
    b1    <- BS.readFile "../record/batch1.bin"
    b10   <- BS.readFile "../record/batch10.bin"
    b100  <- BS.readFile "../record/batch100.bin"
    b1000 <- BS.readFile "../record/batch1000.bin"

    defaultMain $ [
        bgroup "decode" [
            bench "batch1" $ nfIO $ do
                forM [zero..999] $ \_ -> do
                    decode =<< makeReader b1
          , bench "batch10" $ nfIO $ do
                forM [zero..99] $ \_ -> do
                    decode =<< makeReader b10
          , bench "batch100" $ nfIO $ do
                forM [zero..9] $ \_ -> do
                    decode =<< makeReader b100
          , bench "batch1000" $ nfIO $ do
                decode =<< makeReader b1000
          ]
      ]
