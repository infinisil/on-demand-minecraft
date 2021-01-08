{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module State where

import Connection
import DigitalOcean
import Config

import Text.Read
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
import Data.Time

import qualified Data.ByteString.Char8 as BS

type StillUp = Bool

data UpstreamState = Down | Starting DropletId UTCTime | Up StillUp DropletId IpAddress deriving (Show, Read, Eq)

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


updateAndGetState :: forall r . Members '[Reader Config, Async, Final IO, Embed IO, Error String, Trace, AtomicState UpstreamState] r => Sem r UpstreamState
updateAndGetState = do
  trace "Updating upstream state"
  currentState <- atomicGet
  newState <- case currentState of
    Down -> do
      trace "Currently down"
      return Down
    Starting dropletId since -> do
      trace "Currently starting, checking how that's going"
      doStatus <- getDOStatus dropletId
      case doStatus of
        (New, _) -> return $ Starting dropletId since
        (Active, Just ip) -> do
          up <- isServerRunning ip
          return $ if up then
            Up True dropletId ip
          else
            Starting dropletId since
        (_, _) -> return Down
    Up still dropletId ip -> do
      trace "Currently up, checking whether it's still up"
      up <- isServerRunning ip
      if up then
        return $ Up True dropletId ip
      else do
        (status, _) <- getDOStatus dropletId
        return $ case status of
          New -> Up False dropletId ip
          Active -> Up False dropletId ip
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
          let ip = fmap networkIpAddress $ find (\net -> networkType net == "private") $ v4 $ dropletNetworks droplet
          return (status, ip)

dropletPayload :: Member (Reader Config) r => Integer -> Sem r IDropletPayload
dropletPayload imageId = do
  doConfig <- asks digitalOcean
  return $ IDropletPayload
    { dropletpayloadRegion = region doConfig
    , dropletpayloadSize = size doConfig
    , dropletpayloadImage = WithImageId imageId
    , dropletpayloadSshKeys = Just [sshKey doConfig]
    , dropletpayloadBackups = Nothing
    , dropletpayloadIpv6 = Nothing
    , dropletpayloadPrivateNetworking = Just True
    , dropletpayloadUserData = Nothing
    , dropletpayloadMonitoring = Nothing
    , dropletpayloadVolumes = Just [volume doConfig]
    , dropletpayloadTags = Nothing
    }

startUpstream :: Members '[Embed IO, Reader Config, Error String, DigitalOcean, Trace, AtomicState UpstreamState] r => Sem r ()
startUpstream = do
  imageIdStr <- asks (imageFile . digitalOcean) >>= embed . readFile
  imageId <- note ("Couldn't parse image id from" <> imageIdStr) $ readMaybe imageIdStr
  trace "STARTING"
  payload <- dropletPayload imageId
  droplet <- createDroplet "minecraft" payload
  now <- embed getCurrentTime
  atomicPut $ Starting (dropletId droplet) now
