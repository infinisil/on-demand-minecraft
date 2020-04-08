{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main where

import Minecraft

import Network.Socket
import System.IO
import Control.Concurrent (forkIO)
import Control.Monad


statusPacket :: ServerPacket StatusState
statusPacket = ServerPacketResponse $ Response
  { response_version = ResponseVersion "1.15.2" 578
  , response_players = ResponsePlayers 20 0
  , response_description = Chat "Server isn't running"
  }

server :: PacketSender ServerPacket -> PacketReceiver ClientPacket -> IO ()
server send recv = void $ recv @HandshakingState $ \case
  ClientPacketHandshake handshake -> case nextState handshake of
    StatusState -> clientLoopStatus
    LoginState -> clientLoopLogin
  where
    clientLoopStatus :: IO ()
    clientLoopStatus = void $ recv @StatusState $ \case
      ClientPacketRequest -> do
        Just response <- queryServerStatus "infinisil.com"
        send (ServerPacketResponse response)
        clientLoopStatus
      ClientPacketPing nonce -> do
        send $ ServerPacketPong nonce
        clientLoopStatus

    clientLoopLogin :: IO ()
    clientLoopLogin = void $ recv @LoginState $ \case
      ClientPacketLoginStart name -> do
        send $ ServerPacketLoginSuccess name "01e2780a-1334-4891-95dd-506e58dcebb9"
        send $ ServerPacketDisconnect "starting"

runServer :: IO ()
runServer = do
  serverSocket <- socket AF_INET Stream defaultProtocol
  let hints = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  -- TODO: Listen on all addresses to support both IPv4 and IPv6?
  addr <- head <$> getAddrInfo (Just hints) Nothing (Just "25565")
  bind serverSocket $ addrAddress addr
  listen serverSocket 16
  forever $ do
    (socket, addr) <- accept serverSocket
    putStrLn $ "Accepted connection from " <> show addr
    forkIO $ serverProtocolRunner socket server

queryServerStatus :: HostName -> IO (Maybe Response)
queryServerStatus hostname = do
  let hints = defaultHints { addrSocketType = Stream }
  addr <- head <$> getAddrInfo (Just hints) (Just hostname) (Just "25565")
  putStrLn $ "Address is " <> show addr
  clientSocket <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect clientSocket $ addrAddress addr
  clientProtocolRunner clientSocket client

client :: PacketSender ClientPacket -> PacketReceiver ServerPacket -> IO (Maybe Response)
client send recv = do
  send $ ClientPacketHandshake $ Handshake
    { protocolVersion = 578
    , serverAddress = ""
    , serverPort = 0
    , nextState = StatusState
    }
  send ClientPacketRequest
  recv @StatusState $ \case
    ServerPacketResponse response -> return response

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  runServer
