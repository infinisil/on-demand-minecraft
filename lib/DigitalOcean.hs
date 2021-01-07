{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
module DigitalOcean where

import Config

import Network.DigitalOcean
import Network.DigitalOcean.Types
import Network.DigitalOcean.Services
import qualified Data.ByteString.Char8 as BS

import Polysemy
import Polysemy.Error
import Polysemy.Reader

import qualified Control.Monad.Reader as MTLReader
import qualified Control.Monad.Except as MTLExcept

data DigitalOcean (m :: * -> *) a where
  CreateDroplet :: DropletName -> IDropletPayload -> DigitalOcean m Droplet
  GetDroplet :: DropletId -> DigitalOcean m Droplet
  DeleteDroplet :: DropletId -> DigitalOcean m ()

makeSem ''DigitalOcean


runDigitalOcean :: forall r a . Members '[Embed IO, Error DoErr, Reader Config] r => Sem (DigitalOcean ': r) a -> Sem r a
runDigitalOcean = interpret $ \case
  CreateDroplet name payload -> doToSem $ Network.DigitalOcean.createDroplet name payload
  GetDroplet dropletId -> doToSem $ Network.DigitalOcean.getDroplet dropletId
  DeleteDroplet dropletId -> doToSem $ Network.DigitalOcean.deleteDroplet dropletId
  where
  doToSem :: DO x -> Sem r x
  doToSem doAction = do
    token <- asks (tokenFile . digitalOcean) >>= embed . readFile
    fromEitherM $ MTLExcept.runExceptT $ MTLReader.runReaderT (runDO doAction) (Client (BS.pack token))
