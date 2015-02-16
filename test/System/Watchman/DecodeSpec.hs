module System.Watchman.DecodeSpec (main, spec) where

import qualified Data.ByteString        as BS
import           Data.Serialize.Get
import           System.IO              (IOMode (..), hGetBuf, openFile)
import           System.Watchman        hiding (decode)
import           System.Watchman.Cereal
import           Test.Hspec

main :: IO ()
main = hspec spec

-- TODO: how can I un-hardcode this?
getRecordingFileName :: String -> IO String
getRecordingFileName name =
    return $ "/home/cstrahan/src/hwatchman/record/" ++ name

decode :: BS.ByteString -> IO Value
decode str = case go str of
    Left msg  -> fail msg
    Right val -> return val
  where
    go str = flip runGet str $ do
        (_, cont) <- getPduHeader
        cont
        getValue

spec :: Spec
spec = do
    describe "decoding" $ do
        it "works" $ do
            path <- getRecordingFileName "batch1000.bin"
            text <- BS.readFile path
            decoded <- decode text
            putStrLn $ show $ decoded
            1 `shouldBe` 2
