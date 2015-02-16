module System.Watchman.DecodeSpec (main, spec) where

import           System.IO       (IOMode (..), hGetBuf, openFile)
import           System.Watchman
import           Test.Hspec

main :: IO ()
main = hspec spec

-- TODO: how can I un-hardcode this?
getRecordingFileName :: String -> IO String
getRecordingFileName name =
    return $ "/home/cstrahan/src/hwatchman/record/" ++ name

spec :: Spec
spec = do
    describe "decoding" $ do
        it "works" $ do
            path <- getRecordingFileName "batch1000.bin"
            handle <- openFile path ReadMode
            decoded <- decode $ hGetBuf handle
            putStrLn $ show $ decoded
            1 `shouldBe` 2
