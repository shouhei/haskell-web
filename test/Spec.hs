import Lib
import Test.Hspec

specGetServer :: Spec
specGetServer = do
  describe "getServer" $ do
    it "standard" $
      getServer `shouldBe` "haskell-web"

main :: IO ()
main = hspec $ do
  specGetServer
