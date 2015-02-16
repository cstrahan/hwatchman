module System.Watchman.Cereal (
    getPdu
  , getPduHeader
  , readPduSize
  , getValue
)	where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import qualified Data.HashMap             as M
import           Data.Int                 (Int16, Int32, Int64, Int8)
import           Data.Serialize.Get
import qualified Data.Vector              as V
import qualified Data.Vector.Generic      as VG
import           Data.Word                (Word8)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import qualified System.Watchman          as W

data Marker = ArrayMarker
            | MapMarker
            | StringMarker
            | Int8Marker
            | Int16Marker
            | Int32Marker
            | Int64Marker
            | FloatMarker
            | TrueMarker
            | FalseMarker
            | NullMarker
            | TemplateMarker
            | SkipMarker
            deriving (Eq, Show)

-- | Parse an entire PDU.
getPdu :: Get W.Value
getPdu = join $ (>> getValue) . snd <$> getPduHeader

-- | Given a function /read/ that pulls a ByteString from a PDU stream, parse
--   the size of the PDU.
readPduSize :: Monad m => (Int -> m BS.ByteString) -> m (Either String Int)
readPduSize read = do
    bs <- read 3 -- \x00\x01 followed by an integer marker byte
    case runGet getPduHeader bs of
        Right (needed, cont) -> read needed >>= return . runGet cont
        Left msg             -> return . Left $ msg

-- | Parse the PDU header, returning the number of additional bytes required
--   to fetch the size as well as the continuation to parse the the size itself.
getPduHeader :: Get (Int, Get Int)
getPduHeader = do
    byte0 <- getWord8
    byte1 <- getWord8
    marker <- getMarker
    case marker of
        Int8Marker  -> return (1, fromIntegral <$> (getPtr 1 :: Get Int8))
        Int16Marker -> return (2, fromIntegral <$> (getPtr 2 :: Get Int16))
        Int32Marker -> return (4, fromIntegral <$> (getPtr 4 :: Get Int32))
        Int64Marker -> return (8, fromIntegral <$> (getPtr 8 :: Get Int64))

-- | Parse the value part of a PDU.
getValue :: Get W.Value
getValue = do
    marker <- lookAhead getMarker
    case marker of
        ArrayMarker    -> W.Array <$> getArray
        MapMarker      -> W.Object <$> getMap
        StringMarker   -> W.String <$> getString
        Int8Marker     -> W.Int <$> getInt
        Int16Marker    -> W.Int <$> getInt
        Int32Marker    -> W.Int <$> getInt
        Int64Marker    -> W.Int <$> getInt
        FloatMarker    -> W.Double <$> getFloat
        TrueMarker     -> skipMarker >> return (W.Bool True)
        FalseMarker    -> skipMarker >> return (W.Bool False)
        NullMarker     -> skipMarker >> return W.Null
        TemplateMarker -> getTemplate
        _              -> fail "unexpected or invalid marker"

getMarker :: Get Marker
getMarker = do
    byte <- getWord8
    case byte of
        0x00 -> return $ ArrayMarker
        0x01 -> return $ MapMarker
        0x02 -> return $ StringMarker
        0x03 -> return $ Int8Marker
        0x04 -> return $ Int16Marker
        0x05 -> return $ Int32Marker
        0x06 -> return $ Int64Marker
        0x07 -> return $ FloatMarker
        0x08 -> return $ TrueMarker
        0x09 -> return $ FalseMarker
        0x0a -> return $ NullMarker
        0x0b -> return $ TemplateMarker
        0x0c -> return $ SkipMarker
        _    -> fail "invalid marker type"

getInt :: Get Int
getInt = do
    marker <- getMarker
    case marker of
        Int8Marker  -> fromIntegral <$> (getPtr 1 :: Get Int8)
        Int16Marker -> fromIntegral <$> (getPtr 2 :: Get Int16)
        Int32Marker -> fromIntegral <$> (getPtr 4 :: Get Int32)
        Int64Marker -> fromIntegral <$> (getPtr 8 :: Get Int64)
        _           -> fail "unexpected non-integer marker"

getFloat :: Get Double
getFloat = expectMarker FloatMarker >> getPtr (sizeOf (undefined :: Double))

skipMarker :: Get ()
skipMarker = skip (sizeOf (undefined :: Word8))

expectMarker :: Marker -> Get ()
expectMarker expected = do
    actual <- getMarker
    if actual == expected
    then return ()
    else fail $ "expected " ++ show expected

getString :: Get BS.ByteString
getString = expectMarker StringMarker >> getInt >>= getByteString

getArrayHeader :: Get Int
getArrayHeader = expectMarker ArrayMarker >> getInt

getArray :: Get (V.Vector W.Value)
getArray = getArrayHeader >>= flip VG.replicateM getValue

getTemplate :: Get W.Value
getTemplate = do
    expectMarker TemplateMarker
    colCount <- getArrayHeader
    cols <- VG.replicateM colCount getString
    rowCount <- getInt
    fmap W.Array $ VG.replicateM rowCount $ do
        fmap W.Object $ V.foldl' (\acc col -> do
            map <- acc
            marker <- lookAhead getMarker
            if marker == SkipMarker
            then skipMarker >> return map
            else do
                val <- getValue
                return $ M.insert col val map
            ) (return M.empty) cols

getMap :: Get (M.Map BS.ByteString W.Value)
getMap = expectMarker MapMarker >> getInt >>= go
  where
    go size | size <= 0 = return M.empty
            | otherwise = f (size-1) M.empty
      where
        f 0 map = do
            key <- getString
            value <- getValue
            return $ M.insert key value map
        f n map = do
            key <- getString
            value <- getValue
            f (n-1) $ M.insert key value map

getPtr :: Storable a => Int -> Get a
getPtr n = do
    (fp,o,_) <- BS.toForeignPtr <$> getBytes n
    let k p = peek (castPtr (p `plusPtr` o))
    return (BS.inlinePerformIO (withForeignPtr fp k))
{-# INLINE getPtr #-}
