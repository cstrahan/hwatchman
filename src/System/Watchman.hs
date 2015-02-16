{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module System.Watchman (
      decode
    , sizeOfPdu
    ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson                 as J
import           Data.Aeson.Types           (parseMaybe)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Internal   as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Int                   (Int16, Int32, Int64, Int8)
import qualified Data.Int                   as N
import qualified Data.Map                   as M
import qualified Data.Vector                as V
import qualified Data.Vector.Mutable        as MV
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           System.Directory           (findExecutable)
import           System.Exit                (ExitCode (..))
import           System.IO                  (Handle, hGetBuf)
import           System.Process             (readProcessWithExitCode)
import           Control.DeepSeq

--connect :: String -> IO Socket
--  soc <- socket AF_UNIX Stream defaultProtocol
--  connect soc (SockAddrUnix (vmBaseDir </> n </> "monitor.soc"))
--  send soc (c ++ "\n")
--  sClose soc
--
--

-- Constants
arrayMarker    = 0x00 :: Word8
mapMarker      = 0x01 :: Word8
stringMarker   = 0x02 :: Word8
int8Marker     = 0x03 :: Word8
int16Marker    = 0x04 :: Word8
int32Marker    = 0x05 :: Word8
int64Marker    = 0x06 :: Word8
floatMarker    = 0x07 :: Word8
trueVal        = 0x08 :: Word8
falseVal       = 0x09 :: Word8
nullVal        = 0x0a :: Word8
templateMarker = 0x0b :: Word8
skipMarker     = 0x0c :: Word8

int8Size    = sizeOf (undefined :: Int8)
int16Size   = sizeOf (undefined :: Int16)
int32Size   = sizeOf (undefined :: Int32)
int64Size   = sizeOf (undefined :: Int64)
float64Size = sizeOf (undefined :: Double)

binaryMarkerSize = int8Size * 2
sniffBufferSize  = binaryMarkerSize + int8Size

data Value = Array  (V.Vector Value)
           | String BS.ByteString -- No guaranteed encoding, as the underlying system APIs don't define any particular encoding.
           | Int    N.Int      -- TODO: should use Int64, and maybe support others too.
           | Double Double
           | Bool   Bool
           | Object (M.Map BS.ByteString Value)
           | Null
           deriving (Show)

instance NFData Value where
    rnf (Array array)   = rnf array
    rnf (String string) = rnf string
    rnf (Int int)       = rnf int
    rnf (Double double) = rnf double
    rnf (Bool bool)     = rnf bool
    rnf (Object object) = rnf object
    rnf null@(Null)     = null `seq` ()

getSockname :: IO (Maybe String)
getSockname = do
    exe <- findExecutable "watchman"
    case exe of
        Nothing   -> return Nothing
        Just path -> do
            (exitCode, stdout, stderr) <- readProcessWithExitCode path ["get-sockname"] ""
            case exitCode of
                ExitFailure code -> return Nothing
                ExitSuccess      -> return . parseSockname $ stdout
  where
    parseSockname json = do
        result <- J.decode $ BSL.pack json
        flip parseMaybe result $ \obj -> do
            sockname <- obj J..: "sockname"
            return sockname

decode :: (Ptr a -> Int -> IO Int) -> IO Value
decode reader = do
    -- we add the number of bytes consumed to make the offset math simpler.
    (size, offset) <- sizeOfPdu reader
    let endOffset = offset + size
    fptr <- mallocForeignPtrBytes endOffset
    withForeignPtr fptr $ \ptr -> do
        readCount <- reader (plusPtr ptr offset) size
        when (readCount /= size) $ do
            fail "System.Watchman.decodePDU: unexpected end of input"
        doDecode fptr ptr offset endOffset

doDecode :: ForeignPtr Word8 -> Ptr Word8 -> Int -> Int -> IO Value
doDecode fptr ptr offset endOffset = do
    (value, offset) <- decodeValue offset
    return value
  where
    decodeValue :: Int -> IO (Value, Int)
    decodeValue offset = do
        when (offset + int8Size > endOffset) $ do
            fail "System.Watchman.decodeValue: unexpected end of input"
        marker <- peekByteOff ptr offset :: IO Word8
        if | marker == arrayMarker    -> decodeArray offset
           | marker == mapMarker      -> decodeMap offset
           | marker == stringMarker   -> decodeString offset
           | marker >= int8Marker &&
             marker <= int64Marker -> toInt <$> decodeInt offset
           | marker == floatMarker    -> decodeFloat offset
           | marker == trueVal        -> return (Bool True, offset + int8Size)
           | marker == falseVal       -> return (Bool False, offset + int8Size)
           | marker == nullVal        -> return (Null, offset + int8Size)
           | marker == templateMarker -> decodeTemplate offset
           | otherwise                -> fail "System.Watchman.decodeValue: unexpected marker"
      where
        toInt (int, offset) = (Int int, offset)

    decodeInt :: Int -> IO (Int, Int)
    decodeInt offset = do
        when (offset + int8Size > endOffset) $ do
            fail "System.Watchman.decodeInt: unexpected end of input"
        marker <- peekByteOff ptr offset :: IO Word8
        if | marker == int8Marker  -> (,offset + 1 + 1) . fromIntegral <$> (readInt (offset + int8Size) 1 :: IO Int8)
           | marker == int16Marker -> (,offset + 1 + 2) . fromIntegral <$> (readInt (offset + int8Size) 2 :: IO Int16)
           | marker == int32Marker -> (,offset + 1 + 4) . fromIntegral <$> (readInt (offset + int8Size) 4 :: IO Int32)
           | marker == int64Marker -> (,offset + 1 + 8) . fromIntegral <$> (readInt (offset + int8Size) 8 :: IO Int64)
           | otherwise             -> fail "System.Watchman.decodeInt: invalid int marker"
      where
        readInt :: Storable a => Int -> Int -> IO a
        readInt offset size = do
            when (offset + size > endOffset) $ do
                fail "System.Watchman.decodeInt: overrun extracting integer"
            peekByteOff ptr offset

    decodeArrayHeader :: Int -> IO (Int, Int)
    decodeArrayHeader offset = do
        when (offset + int8Size > endOffset) $ do
            fail "System.Watchman.decodeArrayHeader: unexpected end of input"
        marker <- peekByteOff ptr offset :: IO Word8
        when (marker /= arrayMarker) $ do
            fail "System.Watchman.decodeArrayHeader: not an array"
        decodeInt $ offset + int8Size

    decodeArray :: Int -> IO (Value, Int)
    decodeArray offset = do
        (arrayLength, offset) <- decodeArrayHeader offset
        marray <- MV.new arrayLength
        foldM (\offset i -> do
            (value, offset) <- decodeValue offset
            MV.write marray i value
            return offset
            ) offset [0..arrayLength-1]
        array <- V.unsafeFreeze marray
        return (Array array, offset)

    decodeMap :: Int -> IO (Value, Int)
    decodeMap offset = do
        when (offset + int8Size > endOffset) $ do
            fail "System.Watchman.decodeFloat: unexpected end of input"
        marker <- peekByteOff ptr offset :: IO Word8
        when (marker /= mapMarker) $ do
            fail "System.Watchman.decodeMap: not a map"
        (length, offset) <- decodeInt $ offset + int8Size
        (map, offset) <- go offset length
        return (Object map, offset)
      where
        go offset size | size <= 0 = return (M.empty, offset)
                       | otherwise = f (size-1) (M.empty, offset)
          where
            f 0 (map, offset) = do
                (String key, offset) <- decodeString offset
                (value, offset) <- decodeValue offset
                return (M.insert key value map, offset)
            f n (map, offset) = do
                (String key, offset) <- decodeString offset
                (value, offset) <- decodeValue offset
                f (n-1) (M.insert key value map, offset)

    decodeFloat :: Int -> IO (Value, Int)
    decodeFloat offset = do
        when (offset + int8Size > endOffset) $ do
            fail "System.Watchman.decodeFloat: unexpected end of input"
        marker <- peekByteOff ptr offset :: IO Word8
        when (marker /= arrayMarker) $ do
            fail "System.Watchman.decodeFloat: not a float64"
        when (offset + int8Size + float64Size > endOffset) $ do
            fail "System.Watchman.decodeFloat: insuficient string storage"
        double <- peekByteOff ptr (offset + int8Size) :: IO Double
        return (Double double, offset + int8Size + float64Size)

    decodeTemplate :: Int -> IO (Value, Int)
    decodeTemplate offset = do
        when (offset + int8Size > endOffset) $ do
            fail "System.Watchman.decodeTemplate: unexpected end of input"
        marker <- peekByteOff ptr offset :: IO Word8
        when (marker /= templateMarker) $ do
            fail "System.Watchman.decodeTemplate: not a template"
        (headerCount, offset) <- decodeArrayHeader $ offset + int8Size
        (strings, offset) <- decodeStrings offset headerCount
        (rowCount, offset) <- decodeInt offset
        mrows <- MV.new rowCount
        offset <- foldM (\offset i -> do
            (map, offset) <- foldM (\(map, offset) j -> do
                marker <- peekByteOff ptr offset
                if marker == skipMarker
                then return (map, offset + int8Size)
                else do
                    let key = strings V.! j
                    (value, offset) <- decodeValue offset
                    return (M.insert key value map, offset)
                ) (M.empty, offset) [0..headerCount-1]
            MV.write mrows i (Object map)
            return offset
            ) offset [0..rowCount-1]
        rows <- V.unsafeFreeze mrows
        return (Array rows, offset)
      where
        decodeStrings :: Int -> Int -> IO (V.Vector BS.ByteString, Int)
        decodeStrings offset size = do
            mvec <- MV.new size
            offset <- foldM (\offset i -> do
                (String string, offset) <- decodeString (offset)
                MV.write mvec i string
                return offset
                ) offset [0..size-1]
            vec <- V.unsafeFreeze mvec
            return (vec, offset)

    decodeString :: Int -> IO (Value, Int)
    decodeString offset = do
        when (offset + int8Size > endOffset) $ do
            fail "System.Watchman.decodeString: unexpected end of input"
        marker <- peekByteOff ptr offset :: IO Word8
        when (marker /= stringMarker) $ do
            fail "System.Watchman.decodeString: not a string"
        (length, stringOffset) <- decodeInt $ offset + int8Size
        when (stringOffset + length > endOffset) $ do
            fail "System.Watchman.decodeString: insuficient string storage"
        if length == 0
        then return (String $ BS.empty, stringOffset)
        else return (String $ BS.fromForeignPtr fptr stringOffset length, stringOffset + length)

-- TODO: consider returning the header as a ByteString
-- | Returns the of size of the PDU in bytes (excluding the header) and the
-- current number of bytes consumed.
sizeOfPdu :: (Ptr a -> Int -> IO Int) -> IO (Int, Int)
sizeOfPdu reader = do
    allocaBytes sniffBufferSize $ \ptr -> do
        readCount <- reader ptr sniffBufferSize
        when (readCount /= sniffBufferSize) $ do
            fail "System.Watchman.sizeOfPdu: failed to sniff PDU header"
        byte0 <- peekByteOff ptr 0 :: IO Word8
        byte1 <- peekByteOff ptr 1 :: IO Word8
        when (byte0 /= 0 && byte1 /= 1) $ do
            fail "System.Watchman.sizeOfPdu: buffer does not begin with \"\\x00\\x01\" binary marker"
        pduSizeMarker <- peekByteOff ptr binaryMarkerSize :: IO Word8
        if | pduSizeMarker == int8Marker  -> (,sniffBufferSize + 1) . fromIntegral <$> (readSize 1 :: IO Int8)
           | pduSizeMarker == int16Marker -> (,sniffBufferSize + 2) . fromIntegral <$> (readSize 2 :: IO Int16)
           | pduSizeMarker == int32Marker -> (,sniffBufferSize + 4) . fromIntegral <$> (readSize 4 :: IO Int32)
           | pduSizeMarker == int64Marker -> (,sniffBufferSize + 8) . fromIntegral <$> (readSize 8 :: IO Int64)
           | otherwise                    -> fail "System.Watchman.sizeOfPdu: invalid size marker"
  where
    readSize size =
        alloca $ \ptr -> do
            readCount <- reader (castPtr ptr) size
            when (readCount /= size) $ do
                fail "System.Watchman.sizeOfPdu: failed to sniff PDU header"
            peek ptr
