{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import Minecraft
import DigitalOcean
import Config

import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as BS
import System.IO
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.List (find)
import Polysemy
import Polysemy.Async
import Polysemy.Trace
import Polysemy.Error
import Polysemy.Resource
import Polysemy.AtomicState
import Data.Maybe (fromMaybe)
import System.Environment
import Polysemy.Reader
import           Polysemy.Final

import qualified Network.DigitalOcean.Services as DOS
import qualified Network.DigitalOcean.Types as DOT

import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory

import qualified Control.Concurrent.Async as A
import qualified Control.Exception


dropletPayload :: DOS.IDropletPayload
dropletPayload = DOS.IDropletPayload
  { DOS.dropletpayloadRegion = "fra1"
  , DOS.dropletpayloadSize = "c-2"
  , DOS.dropletpayloadImage = DOS.WithImageId 61884377
  , DOS.dropletpayloadSshKeys = Just ["25879389"]
  , DOS.dropletpayloadBackups = Nothing
  , DOS.dropletpayloadIpv6 = Nothing
  , DOS.dropletpayloadPrivateNetworking = Just True
  , DOS.dropletpayloadUserData = Nothing
  , DOS.dropletpayloadMonitoring = Nothing
  , DOS.dropletpayloadVolumes = Just ["48084520-7a5f-11ea-aa42-0a58ac14d120"]
  , DOS.dropletpayloadTags = Nothing
  }

shallowServer :: forall r . Members '[Embed IO, Reader Whitelist, Reader DOT.Client, AtomicState UpstreamState, Async, Final IO, Trace, Minecraft Server] r => UpstreamState -> Sem r ()
shallowServer currentState = receivePacket @HandshakingState >>= \case
  ClientPacketHandshake handshake -> case nextState handshake of
    StatusState -> clientLoopStatus
    LoginState -> clientLoopLogin

  where
    clientLoopStatus :: Sem r ()
    clientLoopStatus = maybeReceivePacket @StatusState >>= \case
      Nothing -> trace "Client disconnected"
      Just ClientPacketRequest -> do
        sendPacket $ ServerPacketResponse $ Response
          { response_version = ResponseVersion "1.15.2" 578
          , response_players = ResponsePlayers (-1) 0
          , response_description = Chat $ case currentState of
              Down -> "Server is down"
              Starting _ -> "Server is starting"
          }
        clientLoopStatus
      Just (ClientPacketPing nonce) -> do
        sendPacket $ ServerPacketPong nonce
        clientLoopStatus

    clientLoopLogin :: Sem r ()
    clientLoopLogin = receivePacket @LoginState >>= \case
      ClientPacketLoginStart name -> do
        muuid <- Map.lookup name <$> ask
        case muuid of
          -- TODO: Figure out if actual servers respond like this
          Nothing -> sendPacket $ ServerPacketDisconnect "You are not allowed to start this server!"
          Just uuid -> do
            sendPacket $ ServerPacketLoginSuccess name uuid
            currentState <- atomicGet
            case currentState of
              Down -> do
                result <- runError @DOT.DoErr $ runDigitalOcean startUpstream
                let message = case result of
                      Left err -> Text.pack $ show err
                      Right _ -> "You're whitelisted, now starting server, please wait"
                sendPacket $ ServerPacketDisconnect message
              Starting _ ->
                sendPacket $ ServerPacketDisconnect "Server starting, please wait"
              Up _ ->
                sendPacket $ ServerPacketDisconnect "Server is now online, try again"

startUpstream :: Members '[DigitalOcean, Trace, AtomicState UpstreamState] r => Sem r ()
startUpstream = do
  trace "STARTING"
  droplet <- createDroplet "minecraft-test" dropletPayload
  atomicPut $ Starting (DOS.dropletId droplet)

tcpRelay :: Member (Embed IO) r => Socket -> Socket -> Sem r ()
tcpRelay a b = embed $ do
  void $ A.race (relay a b) (relay b a)
  close a
  close b
  where
  relay :: Socket -> Socket -> IO ()
  relay from to = loop where
    loop = do
      bytes <- recv from 4096
      unless (BS.null bytes) $ do
        sendAll to bytes
        loop

data UpstreamState = Down | Starting DOT.DropletId | Up (DOT.DropletId, DOT.IpAddress) deriving (Show, Read, Eq)

getUpstreamAddr :: Members '[Error String, Embed IO] r => DOT.IpAddress -> Sem r AddrInfo
getUpstreamAddr ip = do
  let hints = defaultHints { addrSocketType = Stream }
  addresses <- embed $ getAddrInfo (Just hints) (Just ip) (Just "25565")
  case addresses of
    [] -> throw $ "Couldn't get any upstream addresses for ip " <> show ip
    address:_ -> return address


initListenSocket :: Member (Embed IO) r => Sem r Socket
initListenSocket = embed $ do
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  -- TODO: Listen on all addresses to support both IPv4 and IPv6?
  addr <- addrAddress . head <$> getAddrInfo (Just hints) Nothing (Just "25565")

  serverSocket <- socket AF_INET Stream defaultProtocol
  bind serverSocket addr
  listen serverSocket 16
  return serverSocket

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

main :: IO ()
main = runFinal
  $ embedToFinal
  $ resourceToIOFinal
  $ bracket restoreState saveState $ \var -> runAtomicStateTVar var
  $ asyncToIOFinal
  $ traceToIO
  $ do
    result <- errorToIOFinal $ runConfig runServer
    case result of
      Left err -> trace err
      Right result -> return result

runServer :: Members '[Error String, Reader DOT.Client, Trace, Async, Embed IO, Final IO, AtomicState UpstreamState, Reader Whitelist] r => Sem r ()
runServer = do
  serverSocket <- initListenSocket
  trace "Now accepting connections"
  forever $ do
    peer <- embedFinal $ accept serverSocket
    async $ do
      result <- try $ handlePeer peer
      case result of
        Left err -> trace $ "Error during peer handling :" <> err
        Right result -> trace "Finished handling peer successfully"

updateAndGetState :: forall r . Members '[Reader DOT.Client, Async, Final IO, Embed IO, Error String, Trace, AtomicState UpstreamState] r => Sem r UpstreamState
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
        (DOS.New, _) -> return $ Starting dropletId
        (DOS.Active, Just ip) -> do
          up <- isUp ip
          return $ if up then
            Up (dropletId, ip)
          else
            Starting dropletId
        (_, _) -> return Down
    Up (dropletId, ip) -> do
      trace "Currently up, checking whether it's still up"
      up <- isUp ip
      if up then
        return $ Up (dropletId, ip)
      else do
        (status, _) <- getDOStatus dropletId
        return $ case status of
          DOS.New -> Starting dropletId
          DOS.Active -> Starting dropletId
          _ -> Down
  atomicPut newState
  return newState

  where
    getDOStatus :: DOT.DropletId -> Sem r (DOS.DropletStatus, Maybe DOT.IpAddress)
    getDOStatus dropletId = do
      result <- runError $ runDigitalOcean $ getDroplet dropletId
      case result of
        -- TODO: Failed state?
        Left err -> trace (show err) >> return (DOS.Off, Nothing)
        Right droplet ->  do
          let status = DOS.dropletStatus droplet
          trace $ "Status reported by DigitalOcean is: " <> show status
          let ip = fmap DOS.networkIpAddress $ find (\net -> DOS.networkType net == "public") $ DOS.v4 $ DOS.dropletNetworks droplet
          return (status, ip)


isUp :: Members '[Async, Trace, Final IO, Embed IO] r => DOT.IpAddress -> Sem r Bool
isUp ip = do
  trace $ "Trying to determine whether " <> ip <> " has a running minecraft server"
  result <- runError $ do
    trace "Connecting to the ip"
    upstreamSocket <- connectUpstream ip
    trace "Running a minecraft client"
    runMinecraft upstreamSocket mcClient
  case result of
    Left err -> do
      trace $ "Error while trying to determine whether " <> ip <> " is up: " <> show err
      return False
    Right result -> return result
  where
    mcClient :: Members '[Trace, Minecraft Client] r => Sem r Bool
    mcClient = do
      sendPacket $ ClientPacketHandshake $ Handshake
        { protocolVersion = 578
        , serverAddress = ""
        , serverPort = 25565
        , nextState = StatusState
        }
      sendPacket ClientPacketRequest
      receivePacket @StatusState >>= \case
        ServerPacketResponse response -> do
          trace $ show response
          return True

handlePeer :: Members '[Reader Whitelist, Error String, Reader DOT.Client, Async, Trace, Embed IO, Final IO, AtomicState UpstreamState] r => (Socket, SockAddr) -> Sem r ()
handlePeer (peerSocket, peerAddr) = do
  trace $ "Accepted connection from " <> show peerAddr
  state <- updateAndGetState
  case state of
    Up (_, ip) -> do
      trace "Upstream is up, relaying connection"
      runRelay peerSocket ip
    _ -> do
      trace "Upstream is not up, handling the connection locally"
      runMinecraft peerSocket (shallowServer state)

connectUpstream :: Members '[Error String, Embed IO] r => DOT.IpAddress -> Sem r Socket
connectUpstream ip = do
  addr <- getUpstreamAddr ip
  upstreamSocket <- embed $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  fromExceptionVia @Control.Exception.IOException show $ connect upstreamSocket $ addrAddress addr
  return upstreamSocket


runRelay :: Members '[Error String, Embed IO] r => Socket -> DOT.IpAddress -> Sem r ()
runRelay peerSocket ip = do
  upstreamSocket <- connectUpstream ip
  tcpRelay peerSocket upstreamSocket
