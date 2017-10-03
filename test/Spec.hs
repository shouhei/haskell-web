import Lib
import Test.Hspec

specGetServer :: Spec
specGetServer = do
  describe "getServer" $ do
    it "standard" $
      getServer `shouldBe` "haskell-web"

exampleRequestHeader :: String
exampleRequestHeader = do "GET /hogehoge HTTP/1.0 \r\n\
\hogehogehogehogehoge \r\n\
\hugahugahugahuga \r\n\n"

specGetRequestMethod :: Spec
specGetRequestMethod = do
  describe "getRequestMethod" $ do
    it "standard" $
      (getRequestMethod exampleRequestHeader) `shouldBe` "GET"

specGetRequestPath :: Spec
specGetRequestPath = do
  describe "getRequestPath" $ do
    it "standard" $
      (getRequestPath exampleRequestHeader) `shouldBe` "hogehoge"


main :: IO ()
main = hspec $ do
  specGetServer
  specGetRequestMethod
  specGetRequestPath
