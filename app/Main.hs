{-# LANGUAGE DataKinds, OverloadedStrings, DeriveGeneric #-}
module Main where

import Lib
import Options.Declarative
import Control.Monad.Trans
import Control.Exception
import Network.Socket
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe

data Config = Config { allow_host :: String, port :: Int, document_root :: String } deriving (Show, Generic)
instance FromJSON Config
instance ToJSON Config


main :: IO ()
main = run_ server

server :: Flag "c" '["config_file"] "String" "specify config file" (Def "config.json" String)
      -> Cmd "simple web server" ()
server c = liftIO $ do
  conf_file <- readFile (get c)
  case decode (BS.pack conf_file) :: Maybe Config of
    Nothing -> print "Parse error"
    Just conf -> do
      soc <- serveSocket (port conf)
      listen soc 5
      acceptLoop soc `finally` close soc

