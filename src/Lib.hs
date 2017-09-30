module Lib
  (
    serveSocket,
    acceptLoop,
    receiveRequestLoop,
  )
where

import Network.Socket
import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.IORef
import Data.Time
import Data.List.Split

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

getRequestMethod :: String -> String
getRequestMethod request = do
  (words $ head $ lines request) !! 0

getRequestPath :: String -> String
getRequestPath request = do
  tail $ (words $ head $ lines request) !! 1

notFound :: String -> String
notFound d = do
  "HTTP/1.0 404 Not Found\n" ++ "Accept-Ranges: bytes\nContent-Length: 0\n" ++ "Content-Type: text/plan;\n" ++ "Date: " ++ d ++ "\n\n"

addHeader :: String -> String -> String -> String
addHeader d c body = do
  let content_length = length body
  "HTTP/1.0 200 Ok \n\
  \Content-Type: " ++ c ++ "\n\
  \Content-length: " ++ (show content_length) ++ "\n\
  \Date: " ++ d ++ "\n\
  \\n\n" ++ body

getContentType :: String -> String
getContentType file = do
  let extention = (splitOn "." file) !! 1
  if extention == "html" then
    "text/html;charset=utf-8"
  else if extention == "css" then
    "text/css;charset=utf-8"
  else if extention == "js" then
    "application/x-javascript"
  else
    "text/plain"

response :: Socket -> String -> IO()
response conn request = do
  let method = getRequestMethod request
  let path = getRequestPath request
  body <- readFile path
  zt <- getZonedTime
  let today = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0900" zt
  let c = getContentType path
  send conn $ addHeader today c body
  _ <- close conn
  return ()
  `catch` (\(SomeException e) -> do
    zt <- getZonedTime
    let today = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0900" zt
    send conn $ notFound today
    return ()
   )
  `finally` (do
    _ <- close conn
    return ()
  )

