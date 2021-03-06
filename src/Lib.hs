module Lib
  (
    serveSocket,
    acceptLoop,
    receiveRequestLoop,
    getServer,
    getRequestMethod,
    getRequestPath,
    makeHeader,
    getReasonPhrase,
    makeStatusLine,
    getContentType,
    notFound,
    onlyHeader
  )
where

import Network.Socket
import qualified Network.Socket.ByteString as NBS
import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.IORef
import Data.Time
import Data.List.Split
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.IO
import System.Posix.Files
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Codec.Compression.GZip as GZip
import Data.Int as I
import Data.List

serveSocket :: Int -> String -> IO Socket
serveSocket port_int allow_host = do
  let i = (fromIntegral port_int) :: Integer
  let port = (fromInteger i) :: PortNumber
  soc <- socket AF_INET Stream defaultProtocol
  addr <- inet_addr allow_host
  bind soc (SockAddrInet port addr)
  return soc

acceptLoop :: Socket -> IO ()
acceptLoop soc = forever $ do
  (conn, addr) <- accept soc
  forkIO $ receiveRequestLoop conn

receiveRequestLoop :: Socket -> IO ()
receiveRequestLoop conn = do
  request <- newIORef []
  sequence_ $ repeat $ do
    (str, len, _) <- recvFrom conn 64
    modifyIORef request (++ str)
    if len /= 64 then do
      r <- readIORef request
      dispatchRequest conn r
      return ()
    else do
      return ()
  `catch` (\(SomeException e) -> return ())
  `finally` close conn

getServer :: String
getServer = "haskell-web"

getRequestMethod :: String -> String
getRequestMethod request = do
  (words $ head $ lines request) !! 0

getTargetHost :: String -> String
getTargetHost request = do
  let host = find (\x -> (x !! 0) == "Host") $ map (\x -> splitOn ": " x) (lines request)
  case host of
    Nothing -> ""
    Just x -> x !! 1

getRequestPath :: String -> String
getRequestPath request = do
  tail $ (words $ head $ lines request) !! 1

makeHeader :: [(String, String)] -> String
makeHeader kv = do
  foldl (\x y -> x ++ (fst y) ++ ": " ++ (snd y) ++ crlf) "" kv

getReasonPhrase :: Int -> String
getReasonPhrase x
  | x == 200  = "OK"
  | x == 201  = "Created"
  | x == 202  = "Accepted"
  | x == 203  = "Provisional Information"
  | x == 204  = "No Response"
  | x == 205  = "Deleted"
  | x == 206  = "Modified"
  | x == 301  = "Moved Permanently"
  | x == 302  = "Moved Temporarily"
  | x == 303  = "Method"
  | x == 304  = "Not Modified"
  | x == 400  = "Bad Request"
  | x == 401  = "Unauthorized"
  | x == 402  = "Payment Required"
  | x == 403  = "Forbidden"
  | x == 404  = "Not Found"
  | x == 405  = "Method Not Allowed"
  | x == 406  = "None Acceptable"
  | x == 407  = "Proxy Authentication Required"
  | x == 408  = "Request Timeout"
  | x == 500  = "Internal Server Error"
  | x == 501  = "Not Implemented"
  | x == 502  = "Bad Gateway"
  | x == 503  = "Service Unavailable"
  | x == 504  = "Gateway Timeout"
  | otherwise = ""

makeStatusLine :: Float -> Int -> String
makeStatusLine http_version status_code = do
  "HTTP/" ++ (show http_version) ++ " " ++ (show status_code) ++ " " ++ (getReasonPhrase status_code)

crlf :: String
crlf = "\r\n"

methodNotAllowed :: String -> String
methodNotAllowed d = do
  let kv =  [
        {--General Header--}
        ("Date", d),
        {--Response Header--}
        ("Server", getServer),
        {-- Object Header--}
        ("Content-Language", "ja"),
        ("Content-Length", "0"),
        ("Content-Type", "text/plan;")
        ]
  {-- Status Line --}
  (makeStatusLine 1.0 405) ++ crlf ++ (makeHeader kv) ++ crlf

notFound :: String -> String
notFound d = do
  let kv =  [
        {--General Header--}
        ("Date", d),
        {--Response Header--}
        ("Server", getServer),
        {-- Object Header--}
        ("Content-Language", "ja"),
        ("Content-Length", "0"),
        ("Content-Type", "text/plan;")
        ]
  {-- Status Line --}
  (makeStatusLine 1.0 404) ++ crlf ++ (makeHeader kv) ++ crlf

onlyHeader :: String -> String -> String -> I.Int64 -> String
onlyHeader date last content_type content_length = do
  let kv = [
        {--General Header --}
        ("Date", date),
        {-- Response Header --}
        ("Server", getServer),
        {-- Object Header --}
        ("Content-Language", "ja"),
        ("Content-Encoding", "gzip"),
        ("Last-Modified", last),
        ("Content-Type", content_type),
        ("Content-Length", (show content_length))
        ]
  {--Status Line--}
  (makeStatusLine 1.0 200) ++ crlf ++ (makeHeader kv) ++ crlf

getContentType :: String -> String
getContentType file
  | extention == "html"                       = "text/html;charset=utf-8"
  | extention == "css"                        = "text/css;charset=utf-8"
  | extention == "js"                         = "application/x-javascript"
  | extention == "gif"                        = "image/gif"
  | extention == "jpg" || extention == "jpeg" = "image/jpeg"
  | extention == "png"                        = "image/png"
  | otherwise                                 = "text/plain"
  where extention = (splitOn "." file) !! 1

sendAllData :: Socket -> BS.ByteString -> IO()
sendAllData conn content
  | (BS.length content) == 0 = close conn
  | otherwise = do
      send_data <- NBS.send conn content
      let remaining_content = BS.drop send_data content
      sendAllData conn remaining_content

httpDateFormat :: String
httpDateFormat = "%a, %d %b %Y %H:%M:%S GMT"

getLastModified :: String -> IO String
getLastModified path = do
  last_modified_epoch <- modificationTime <$> getFileStatus path
  return $ formatTime defaultTimeLocale httpDateFormat $ posixSecondsToUTCTime $ realToFrac last_modified_epoch

today :: IO String
today = do
  utc <- getCurrentTime
  return $ formatTime defaultTimeLocale httpDateFormat utc

defaultResponseCreator :: String -> IO BS.ByteString
defaultResponseCreator request = do
  let method = getRequestMethod request
  let path = getRequestPath request
  let c = getContentType path
  handler <- openBinaryFile path ReadMode
  contents <- hGetContents handler
  last_modified <- getLastModified path
  t <- today
  if method == "HEAD" then do
    let compressed = GZip.compress $ LBS.pack contents
    return $ BS.pack $ onlyHeader t last_modified c (LBS.length compressed)
  else if method == "GET" then do
    let compressed = GZip.compress $ LBS.pack contents
    return $ LBS.toStrict $ LBS.append (LBS.pack (onlyHeader t last_modified c (LBS.length compressed))) compressed
  else do
    return $ BS.pack $ methodNotAllowed t


dispatchRequest :: Socket -> String -> IO()
dispatchRequest conn request = do
  response_data <- defaultResponseCreator request
  sendAllData conn response_data
  `catch` (\(SomeException e) -> do
    print e
    t <- today
    let response_data = (BS.pack (notFound t))
    sendAllData conn response_data
   )
  `finally` (do
    return ()
  )

