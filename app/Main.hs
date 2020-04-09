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

import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import System.IO
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Polysemy
import Polysemy.Async
import Polysemy.Trace
import Polysemy.Error
import Polysemy.Resource
import Polysemy.AtomicState
import Polysemy.Reader
import           Polysemy.Final

import Control.Concurrent.STM
import Control.Monad (join)
import qualified System.Timeout
import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory

import qualified Control.Concurrent.Async as A
import qualified Control.Exception


data Timeout m a where
  Timeout :: Int -> m a -> Timeout m (Maybe a)

makeSem ''Timeout

runTimeout :: Member (Final IO) r => Sem (Timeout ': r) a -> Sem r a
runTimeout = interpretFinal $ \case
  Timeout micros action -> do
    ins <- getInspectorS
    m' <- runS action
    liftS $ join <$> System.Timeout.timeout micros (inspect ins <$> m')

shallowServer :: forall r . Members '[Reader Config, AtomicState UpstreamState, Async, Final IO, Trace, Minecraft Server] r => UpstreamState -> Sem r ()
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
              Starting -> "Server is starting"
          }
        clientLoopStatus
      Just (ClientPacketPing nonce) -> do
        sendPacket $ ServerPacketPong nonce
        clientLoopStatus

    clientLoopLogin :: Sem r ()
    clientLoopLogin = receivePacket @LoginState >>= \case
      ClientPacketLoginStart name -> do
        muuid <- Map.lookup name . config_whitelist <$> ask
        case muuid of
          -- TODO: Figure out if actual servers respond like this
          Nothing -> sendPacket $ ServerPacketDisconnect "You're not whitelisted"
          Just uuid -> do
            startUpstream
            sendPacket $ ServerPacketLoginSuccess name uuid
            sendPacket $ ServerPacketDisconnect $ case currentState of
              Down -> "You're whitelisted, now starting server, please wait"
              Starting -> "Server starting, please wait"

startUpstream :: Members '[Trace, AtomicState UpstreamState] r => Sem r ()
startUpstream = do
  -- TODO: Check if already starting, if not, make it so
  needsStarting <- atomicState $ \case
    Down -> (Starting, True)
    other -> (other, False)
  when needsStarting $ do
    trace "Would start upstream now"

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

data UpstreamState = Down | Starting | Up deriving (Show, Read)

type Whitelist = Map Text Text

data Config = Config
  { config_whitelist :: Whitelist
  , config_upstream :: AddrInfo
  }

whitelist :: Whitelist
whitelist = Map.fromList
  [ ("infinisil", "01e2780a-1334-4891-95dd-506e58dcebb9")
  ]

getUpstreamAddr :: Member (Embed IO) r => Sem r AddrInfo
getUpstreamAddr = do
  let hints = defaultHints { addrSocketType = Stream }
  embed $ head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "25566")


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
    value <- embed $ read <$> readFile statePath
    embed $ newTVarIO value
  else embed $ newTVarIO Down

saveState :: Member (Embed IO) r => TVar UpstreamState -> Sem r ()
saveState state = do
  value <- embed $ readTVarIO state
  embed $ writeFile statePath $ show value

main :: IO ()
main = runFinal $ asyncToIOFinal $ resourceToIOFinal $ embedToFinal $ traceToIO $ do
  embed $ hSetBuffering stdout LineBuffering
  upstream <- getUpstreamAddr
  bracket restoreState saveState
    $ \state -> runReader (Config whitelist upstream)
    $ runAtomicStateTVar state runServer

runServer :: Members '[Trace, Async, Embed IO, Final IO, AtomicState UpstreamState, Reader Config] r => Sem r ()
runServer = do
  serverSocket <- initListenSocket
  forever $ do
    peer <- embedFinal $ accept serverSocket
    async $ do
      result <- runError $ handlePeer peer
      case result of
        Left err -> trace $ "Errur during peer handling :" <> err
        Right result -> trace "Finished handling peer successfully"

updateAndGetState :: Members '[Async, Reader Config, Final IO, Embed IO, Error String, Trace, AtomicState UpstreamState] r => Sem r UpstreamState
updateAndGetState = do
  currentState <- atomicGet
  up <- isUp
  let newState = case (up, currentState) of
        (True, _) -> Up
        (False, Starting) -> Starting
        (False, _) -> Down
  atomicPut newState
  return newState


isUp :: Members '[Async, Trace, Final IO, Embed IO, Error String, Reader Config] r => Sem r Bool
isUp = do
  trace "Trying to determine whether upstream is up"
  result <- runTimeout $ timeout 1000000 $ do
    upstreamSocket <- connectUpstream
    runMinecraft upstreamSocket mcClient
  case result of
    Nothing -> do
      trace "Timed or errored out, upstream is down"
      return False
    Just res -> do
      trace $ "Didn't time out, upstream is up: " <> show res
      return res
  where
    mcClient :: Members '[Trace, Minecraft Client] r => Sem r Bool
    mcClient = do
      sendPacket $ ClientPacketHandshake $ Handshake
        { protocolVersion = 578
        , serverAddress = ""
        , serverPort = 25566
        , nextState = StatusState
        }
      sendPacket ClientPacketRequest
      receivePacket @StatusState >>= \case
        ServerPacketResponse response -> do
          trace $ show response
          return True

handlePeer :: Members '[Error String, Async, Trace, Embed IO, Final IO, AtomicState UpstreamState, Reader Config] r => (Socket, SockAddr) -> Sem r ()
handlePeer (peerSocket, peerAddr) = do
  trace $ "Accepted connection from " <> show peerAddr
  state <- updateAndGetState
  case state of
    Up -> do
      trace "Upstream is up, relaying connection"
      runRelay peerSocket
    _ -> do
      trace "Upstream is not up, handling the connection locally"
      runMinecraft peerSocket (shallowServer state)

connectUpstream :: Members '[Error String, Embed IO, Reader Config] r => Sem r Socket
connectUpstream = do
  addr <- asks config_upstream
  upstreamSocket <- embed $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  fromExceptionVia @Control.Exception.SomeException show $ connect upstreamSocket $ addrAddress addr
  return upstreamSocket


runRelay :: Members '[Error String, Embed IO, Reader Config] r => Socket -> Sem r ()
runRelay peerSocket = do
  upstreamSocket <- connectUpstream
  tcpRelay peerSocket upstreamSocket
