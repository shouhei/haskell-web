{-# LANGUAGE DataKinds #-}
module Main where

import Lib
import Options.Declarative
import Control.Monad.Trans
import Control.Exception
import Network.Socket

main :: IO ()
main = run_ server

server :: Flag "p" '["port"] "INT" "specify server port" (Def "8080" Int)
      -> Cmd "simple web server" ()
server port = liftIO $ do
  soc <- serveSocket (get port)
  listen soc 5
  acceptLoop soc `finally` close soc

