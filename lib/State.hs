{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module State where

import Connection
import DigitalOcean

import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Trace
import Polysemy.Reader
import Polysemy.AtomicState
import Polysemy.Resource
import Control.Concurrent.STM
import Network.DigitalOcean.Types
import Network.DigitalOcean.Services
import System.Directory
import Data.List (find)

import qualified Data.ByteString.Char8 as BS

data UpstreamState = Down | Starting DropletId | Up (DropletId, IpAddress) deriving (Show, Read, Eq)

statePath :: FilePath
statePath = "upstreamstate"

restoreState :: Member (Embed IO) r => Sem r (TVar UpstreamState)
restoreState = do
  exists <- embed $ doesFileExist statePath
  if exists then do
    value <- embed $ read . BS.unpack <$> BS.readFile statePath
    embed $ newTVarIO value
  else embed $ newTVarIO Down

saveState :: Member (Embed IO) r => TVar UpstreamState -> Sem r ()
saveState state = do
  value <- embed $ readTVarIO state
  embed $ BS.writeFile statePath $ BS.pack $ show value

runUpstreamState :: Members '[Resource, Embed IO] r => Sem (AtomicState UpstreamState ': r) a -> Sem r a
runUpstreamState action = bracket restoreState saveState $ \var -> runAtomicStateTVar var action


updateAndGetState :: forall r . Members '[Reader Client, Async, Final IO, Embed IO, Error String, Trace, AtomicState UpstreamState] r => Sem r UpstreamState
updateAndGetState = do
  trace "Updating upstream state"
  currentState <- atomicGet
  newState <- case currentState of
    Down -> do
      trace "Currently down"
      return Down
    Starting dropletId -> do
      trace "Currently starting, checking how that's going"
      doStatus <- getDOStatus dropletId
      case doStatus of
        (New, _) -> return $ Starting dropletId
        (Active, Just ip) -> do
          up <- isServerRunning ip
          return $ if up then
            Up (dropletId, ip)
          else
            Starting dropletId
        (_, _) -> return Down
    Up (dropletId, ip) -> do
      trace "Currently up, checking whether it's still up"
      up <- isServerRunning ip
      if up then
        return $ Up (dropletId, ip)
      else do
        (status, _) <- getDOStatus dropletId
        return $ case status of
          New -> Starting dropletId
          Active -> Starting dropletId
          _ -> Down
  atomicPut newState
  return newState

  where
    getDOStatus :: DropletId -> Sem r (DropletStatus, Maybe IpAddress)
    getDOStatus dropletId = do
      result <- runError $ runDigitalOcean $ getDroplet dropletId
      case result of
        -- TODO: Failed state?
        Left err -> trace (show err) >> return (Off, Nothing)
        Right droplet ->  do
          let status = dropletStatus droplet
          trace $ "Status reported by DigitalOcean is: " <> show status
          let ip = fmap networkIpAddress $ find (\net -> networkType net == "public") $ v4 $ dropletNetworks droplet
          return (status, ip)

dropletPayload :: IDropletPayload
dropletPayload = IDropletPayload
  { dropletpayloadRegion = "fra1"
  , dropletpayloadSize = "c-2"
  , dropletpayloadImage = WithImageId 61884377
  , dropletpayloadSshKeys = Just ["25879389"]
  , dropletpayloadBackups = Nothing
  , dropletpayloadIpv6 = Nothing
  , dropletpayloadPrivateNetworking = Just True
  , dropletpayloadUserData = Nothing
  , dropletpayloadMonitoring = Nothing
  , dropletpayloadVolumes = Just ["48084520-7a5f-11ea-aa42-0a58ac14d120"]
  , dropletpayloadTags = Nothing
  }

startUpstream :: Members '[DigitalOcean, Trace, AtomicState UpstreamState] r => Sem r ()
startUpstream = do
  trace "STARTING"
  droplet <- createDroplet "minecraft-test" dropletPayload
  atomicPut $ Starting (dropletId droplet)
