module Lib
  (
    serveSocket,
    acceptLoop,
    receiveRequestLoop,
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
import System.IO

serveSocket :: Int -> IO Socket
serveSocket port_int = do
  let i = (fromIntegral port_int) :: Integer
  let port = (fromInteger i) :: PortNumber
  soc <- socket AF_INET Stream defaultProtocol
  addr <- inet_addr "0.0.0.0"
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
      response conn r
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

getRequestPath :: String -> String
getRequestPath request = do
  tail $ (words $ head $ lines request) !! 1

notFound :: String -> String
notFound d = do
  "HTTP/1.0 404 Not Found \r\n\
  \Server: " ++  getServer ++" \r\n\
  \Accept-Ranges: bytes\nContent-Length: 0 \r\n\
  \Content-Type: text/plan; \r\n\
  \Date: " ++ d ++ "\r\n\n"

addHeader :: String -> String -> String -> String
addHeader d c body = do
  let content_length = length body
  "HTTP/1.0 200 Ok \r\n\
  \Server:" ++  getServer ++" \r\n\
  \Content-Type: " ++ c ++ " \r\n\
  \Content-length: " ++ (show content_length) ++ "\r\n\
  \Date: " ++ d ++ "\r\n\
  \\r\n" ++ body

getContentType :: String -> String
getContentType file = do
  let extention = (splitOn "." file) !! 1
  if extention == "html" then
    "text/html;charset=utf-8"
  else if extention == "css" then
    "text/css;charset=utf-8"
  else if extention == "js" then
    "application/x-javascript"
  else if extention == "gif" then
    "image/gif"
  else if extention == "jpg" || extention == "jpeg" then
    "image/jpeg"
  else if extention == "png" then
    "image/png"
  else
    "text/plain"


getFileHandler file = do
  let extention = (splitOn "." file) !! 1
  if extention == "html" then
    openFile
  else if extention == "css" then
    openFile
  else if extention == "js" then
    openFile
  else if extention == "gif" then
    openBinaryFile
  else if extention == "jpg" || extention == "jpeg" then
    openBinaryFile
  else if extention == "png" then
    openBinaryFile
  else
    openFile

sendAllData :: Socket -> BS.ByteString -> IO()
sendAllData conn content
  | (BS.length content) == 0 = close conn
  | otherwise = do
      send_data <- NBS.send conn content
      let remaining_content = BS.drop send_data content
      sendAllData conn remaining_content

response :: Socket -> String -> IO()
response conn request = do
  let method = getRequestMethod request
  let path = getRequestPath request
  zt <- getZonedTime
  let logDate = formatTime defaultTimeLocale "[%d/%b/%Y %H:%M:%S]" zt
  let firstLine = init $ (lines request) !! 0
  putStrLn $ firstLine ++ " " ++ logDate
  let f = getFileHandler path
  handler <- f path ReadMode
  contents <- hGetContents handler
  let today = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0900" zt
  let c = getContentType path
  let response_data = addHeader today c contents
  sendAllData conn (BS.pack response_data)
  return ()
  `catch` (\(SomeException e) -> do
    print e
    zt <- getZonedTime
    let today = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0900" zt
    sendAllData conn (BS.pack (notFound today))
    return ()
   )
  `finally` (do
    return ()
  )

