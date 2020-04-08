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
import Control.Exception
import System.IO
import Control.Concurrent (forkIO)
import Control.Monad
import Polysemy
import Polysemy.Async
import Polysemy.Trace
import Polysemy.Error
import Control.Concurrent.STM

import qualified Control.Concurrent.Async as A


shallowServer :: forall r . Members '[Async, Final IO, Trace, Minecraft Server] r => Sem r ()
shallowServer = receivePacket @HandshakingState >>= \case
  ClientPacketHandshake handshake -> case nextState handshake of
    StatusState -> clientLoopStatus
    LoginState -> clientLoopLogin

  where
    clientLoopStatus :: Sem r ()
    clientLoopStatus = maybeReceivePacket @StatusState >>= \case
      Nothing -> trace "Client disconnected"
      Just ClientPacketRequest -> do
        result <- runError $ queryServerStatus "infinisil.com"
        case result of
          Left err -> do
            sendPacket $ ServerPacketResponse $ Response
              { response_version = ResponseVersion "1.15.2" 578
              , response_players = ResponsePlayers (-1) 0
              , response_description = Chat "Error"
              }
            clientLoopStatus
          Right response -> do
            sendPacket $ ServerPacketResponse response
            clientLoopStatus
      Just (ClientPacketPing nonce) -> do
        sendPacket $ ServerPacketPong nonce
        clientLoopStatus

    clientLoopLogin :: Sem r ()
    clientLoopLogin = receivePacket @LoginState >>= \case
      ClientPacketLoginStart name -> do
        sendPacket $ ServerPacketLoginSuccess name "01e2780a-1334-4891-95dd-506e58dcebb9"
        sendPacket $ ServerPacketDisconnect "starting"

runServer :: Members '[Trace, Async, Final IO] r => Sem r ()
runServer = do
  serverSocket <- embedFinal $ socket AF_INET Stream defaultProtocol
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  -- TODO: Listen on all addresses to support both IPv4 and IPv6?
  addr <- embedFinal $ head <$> getAddrInfo (Just hints) Nothing (Just "25565")
  embedFinal $ bind serverSocket $ addrAddress addr
  embedFinal $ listen serverSocket 16
  forever $ do
    (socket, addr) <- embedFinal $ accept serverSocket
    trace $ "Accepted connection from " <> show addr
    async $ runError $ runMinecraft socket shallowServer

relayServer :: Members '[Trace, Async, Final IO] r => Sem r ()
relayServer = do
  serverSocket <- embedFinal $ socket AF_INET Stream defaultProtocol
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  -- TODO: Listen on all addresses to support both IPv4 and IPv6?
  addr <- embedFinal $ head <$> getAddrInfo (Just hints) Nothing (Just "25565")
  embedFinal $ bind serverSocket $ addrAddress addr
  embedFinal $ listen serverSocket 16
  forever $ do
    (serverSocketAccepted, addr) <- embedFinal $ accept serverSocket
    trace $ "Accepted connection from " <> show addr

    let hints = defaultHints { addrSocketType = Stream }
    addr <- embedFinal $ head <$> getAddrInfo (Just hints) (Just "infinisil.com") (Just "25565")
    trace $ "Address is " <> show addr
    clientSocket <- embedFinal $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    embedFinal $ connect clientSocket $ addrAddress addr

    embedFinal $ tcpRelay serverSocketAccepted clientSocket

queryServerStatus :: Members '[Trace, Async, Final IO, Error String] r => HostName -> Sem r Response
queryServerStatus hostname = do
  let hints = defaultHints { addrSocketType = Stream }
  addr <- embedFinal $ head <$> getAddrInfo (Just hints) (Just hostname) (Just "25565")
  trace $ "Address is " <> show addr
  clientSocket <- embedFinal $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  embedFinal $ connect clientSocket $ addrAddress addr
  runMinecraft clientSocket statusQueryClient

statusQueryClient :: Member (Minecraft Client) r => Sem r Response
statusQueryClient = do
  sendPacket $ ClientPacketHandshake $ Handshake
    { protocolVersion = 578
    , serverAddress = ""
    , serverPort = 0
    , nextState = StatusState
    }
  sendPacket ClientPacketRequest
  receivePacket @StatusState >>= \case
    ServerPacketResponse response -> return response

tcpRelay :: Socket -> Socket -> IO ()
tcpRelay a b = do
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  runFinal $ asyncToIOFinal $ embedToFinal $ traceToIO runServer
