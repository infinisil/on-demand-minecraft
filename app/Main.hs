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
  , response_players = ResponsePlayers 20 0 []
  , response_description = Chat "Server isn't running"
  }

server :: ServerPacketSender -> ServerPacketReceiver -> IO ()
server send recv = recv @HandshakingState $ \case
  ClientPacketHandshake handshake -> case nextState handshake of
    StatusState -> clientLoopStatus
    LoginState -> clientLoopLogin
  where
    clientLoopStatus :: IO ()
    clientLoopStatus = recv @StatusState $ \case
      ClientPacketRequest -> do
        send statusPacket
        clientLoopStatus
      ClientPacketPing nonce -> do
        send $ ServerPacketPong nonce
        clientLoopStatus

    clientLoopLogin :: IO ()
    clientLoopLogin = recv @LoginState $ \case
      ClientPacketLoginStart name -> do
        send $ ServerPacketLoginSuccess name "01e2780a-1334-4891-95dd-506e58dcebb9"
        send $ ServerPacketDisconnect "starting"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
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
