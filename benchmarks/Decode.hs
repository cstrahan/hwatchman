{-# LANGUAGE ImpredicativeTypes #-}

import           Control.Monad
import           Criterion.Main
import qualified Data.Aeson             as J
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.Serialize.Get
import           System.Watchman        hiding (decode)
import           System.Watchman.Cereal

-- just to help with inference
zero :: Int
zero = 0

decode :: BS.ByteString -> IO Value
decode str = case go str of
    Left msg  -> fail msg
    Right val -> return val
  where
    go = runGet getPdu

main :: IO ()
main = do
    b1    <- BS.readFile "../record/batch1.bin"
    b10   <- BS.readFile "../record/batch10.bin"
    b100  <- BS.readFile "../record/batch100.bin"
    b1000 <- BS.readFile "../record/batch1000.bin"

    defaultMain $ [
        bgroup "decode bser" [
            bench "batch1" $ nfIO $ do
                forM [zero..999] $ \_ -> do
                    decode b1
          , bench "batch10" $ nfIO $ do
                forM [zero..99] $ \_ -> do
                    decode b10
          , bench "batch100" $ nfIO $ do
                forM [zero..9] $ \_ -> do
                    decode b100
          , bench "batch1000" $ nfIO $ do
                decode b1000
          ]
      ]

--parseJson :: BL.ByteString -> IO J.Value
--parseJson = return . unMaybe . J.decode
--  where
--    unMaybe (Just val) = val
--
--main :: IO ()
--main = do
--    b1    <- BL.readFile "../record/batch1.json"
--    b10   <- BL.readFile "../record/batch10.json"
--    b100  <- BL.readFile "../record/batch100.json"
--    b1000 <- BL.readFile "../record/batch1000.json"
--
--    defaultMain $ [
--        bgroup "decode json" [
--            bench "batch1" $ nfIO $ do
--                forM [zero..999] $ \_ -> do
--                    parseJson b1
--          , bench "batch10" $ nfIO $ do
--                forM [zero..99] $ \_ -> do
--                    parseJson b10
--          , bench "batch100" $ nfIO $ do
--                forM [zero..9] $ \_ -> do
--                    parseJson b100
--          , bench "batch1000" $ nfIO $ do
--                parseJson b1000
--          ]
--      ]
