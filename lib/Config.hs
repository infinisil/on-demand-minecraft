{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Config where

import Polysemy
import Polysemy.Trace
import Polysemy.Reader
import Polysemy.Error
import GHC.Generics
import Data.Aeson

import Network.DigitalOcean.Types (Client(..))
import Data.Map (Map)
import Data.Text (Text)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS

type Whitelist = Map Text Text

data Config = Config
  { whitelist :: Whitelist
  , digitalOceanToken :: String
  } deriving (Show, Generic)

instance FromJSON Config

runConfig :: Members '[Error String, Trace, Embed IO] r => Sem (Reader Whitelist ': Reader Client ': r) a -> Sem r a
runConfig action = do
  args <- embed getArgs
  case args of
    [configFile] -> do
      decoded <- embed $ eitherDecodeFileStrict' configFile
      case decoded of
        Right Config { .. } -> do
          trace $ "Successfully read config file at " <> configFile
          let client = Client (BS.pack digitalOceanToken)
          runReader client $ runReader whitelist action
        Left err -> throw $ "Error while trying to read the config: " <> err
    _ -> throw "Expecting a single argument for the config file"
