{-# LANGUAGE ConstraintKinds #-}
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

import Data.Map (Map)
import Data.Text (Text)
import System.Environment (getArgs)

type Whitelist = Map Text Text

data DigitalOceanConfig = DigitalOceanConfig
  { tokenFile :: FilePath
  , imageFile :: FilePath
  , region :: String
  , size :: String
  , sshKey :: String
  , volume :: String
  } deriving (Show, Generic)

data Config = Config
  { whitelist :: Whitelist
  , digitalOcean :: DigitalOceanConfig
  } deriving (Show, Generic)

instance FromJSON DigitalOceanConfig
instance FromJSON Config

runConfig :: Members '[Error String, Trace, Embed IO] r => Sem (Reader Config ': r) a -> Sem r a
runConfig action = do
  args <- embed getArgs
  case args of
    [configFile] -> do
      decoded <- embed $ eitherDecodeFileStrict' configFile
      case decoded of
        Right config -> do
          trace $ "Successfully read config file at " <> configFile
          runReader config action
        Left err -> throw $ "Error while trying to read the config: " <> err
    _ -> throw "Expecting a single argument for the config file"
