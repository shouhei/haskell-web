import Lib
import Test.Hspec

{--
    makeHeader,
    notFound,
    addHeader
--}

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

specGetReasonPhrase :: Spec
specGetReasonPhrase = do
  describe "getReasonPhrase" $ do
    it "200" $
      (getReasonPhrase 200) `shouldBe` "OK"
    it "201" $
      (getReasonPhrase 201) `shouldBe` "Created"
    it "202" $
      (getReasonPhrase 202) `shouldBe` "Accepted"
    it "203" $
      (getReasonPhrase 203) `shouldBe` "Provisional Information"
    it "204" $
      (getReasonPhrase 204) `shouldBe` "No Response"
    it "205" $
      (getReasonPhrase 205) `shouldBe` "Deleted"
    it "206" $
      (getReasonPhrase 206) `shouldBe` "Modified"
    it "301" $
      (getReasonPhrase 301) `shouldBe` "Moved Permanently"
    it "302" $
      (getReasonPhrase 302) `shouldBe` "Moved Temporarily"
    it "303" $
      (getReasonPhrase 303) `shouldBe` "Method"
    it "304" $
      (getReasonPhrase 304) `shouldBe` "Not Modified"
    it "400" $
      (getReasonPhrase 400) `shouldBe` "Bad Request"
    it "401" $
      (getReasonPhrase 401) `shouldBe` "Unauthorized"
    it "402" $
      (getReasonPhrase 402) `shouldBe` "Payment Required"
    it "403" $
      (getReasonPhrase 403) `shouldBe` "Forbidden"
    it "404" $
      (getReasonPhrase 404) `shouldBe` "Not Found"
    it "405" $
      (getReasonPhrase 405) `shouldBe` "Method Not Allowed"
    it "406" $
      (getReasonPhrase 406) `shouldBe` "None Acceptable"
    it "407" $
      (getReasonPhrase 407) `shouldBe` "Proxy Authentication Required"
    it "408" $
      (getReasonPhrase 408) `shouldBe` "Request Timeout"
    it "500" $
      (getReasonPhrase 500) `shouldBe` "Internal Server Error"
    it "501" $
      (getReasonPhrase 501) `shouldBe` "Not Implemented"
    it "502" $
      (getReasonPhrase 502) `shouldBe` "Bad Gateway"
    it "503" $
      (getReasonPhrase 503) `shouldBe` "Service Unavailable"
    it "504" $
      (getReasonPhrase 504) `shouldBe` "Gateway Timeout"
    it "otherwise" $
      (getReasonPhrase 505) `shouldBe` ""

specMakeStatusLine :: Spec
specMakeStatusLine = do
  describe "makeStatusLine" $ do
    it "standard" $
      makeStatusLine 1.0 200 `shouldBe` "HTTP/1.0 200 OK"

specGetContentType :: Spec
specGetContentType = do
  describe "getContentType" $ do
    it "html" $
      getContentType "sample.html" `shouldBe` "text/html;charset=utf-8"
    it "css" $
      getContentType "sample.css" `shouldBe` "text/css;charset=utf-8"
    it "js" $
      getContentType "sample.js" `shouldBe` "application/x-javascript"
    it "gif" $
      getContentType "sample.gif" `shouldBe` "image/gif"
    it "jpg" $
      getContentType "sample.jpg" `shouldBe` "image/jpeg"
    it "jpeg" $
      getContentType "sample.jpeg" `shouldBe` "image/jpeg"
    it "png" $
      getContentType "sample.png" `shouldBe` "image/png"
    it "otherwise" $
      getContentType "sample.txt" `shouldBe` "text/plain"

main :: IO ()
main = hspec $ do
  specGetServer
  specGetRequestMethod
  specGetRequestPath
  specGetReasonPhrase
  specMakeStatusLine
  specGetContentType
