{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Polysemy
import Polysemy.Async
import Polysemy.Trace

statusPacket :: ServerPacket StatusState
statusPacket = ServerPacketResponse $ Response
  { response_version = ResponseVersion "1.15.2" 578
  , response_players = ResponsePlayers 20 0
  , response_description = Chat "Server isn't running"
  }

server :: forall r . Members '[Trace, Async, Final IO] r => PacketSender ServerPacket r -> PacketReceiver ClientPacket r -> Sem r ()
server send recv = void $ recv @HandshakingState $ \case
  ClientPacketHandshake handshake -> case nextState handshake of
    StatusState -> clientLoopStatus
    LoginState -> clientLoopLogin
  where
    clientLoopStatus :: Sem r ()
    clientLoopStatus = void $ recv @StatusState $ \case
      ClientPacketRequest -> do
        result <- queryServerStatus "infinisil.com"
        case result of
          Just response -> do
            send (ServerPacketResponse response)
            clientLoopStatus
      ClientPacketPing nonce -> do
        send $ ServerPacketPong nonce
        clientLoopStatus

    clientLoopLogin :: Sem r ()
    clientLoopLogin = void $ recv @LoginState $ \case
      ClientPacketLoginStart name -> do
        send $ ServerPacketLoginSuccess name "01e2780a-1334-4891-95dd-506e58dcebb9"
        send $ ServerPacketDisconnect "starting"

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
    --putStrLn $ "Accepted connection from " <> show addr
    async $ runSocketAction socket $ runByteBufferAction $ serverProtocolRunner server

queryServerStatus :: Members '[Trace, Async, Final IO] r => HostName -> Sem r (Maybe Response)
queryServerStatus hostname = do
  let hints = defaultHints { addrSocketType = Stream }
  addr <- embedFinal $ head <$> getAddrInfo (Just hints) (Just hostname) (Just "25565")
  --putStrLn $ "Address is " <> show addr
  clientSocket <- embedFinal $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  embedFinal $ connect clientSocket $ addrAddress addr
  runSocketAction clientSocket $ runByteBufferAction $ clientProtocolRunner client

client :: PacketSender ClientPacket r -> PacketReceiver ServerPacket r -> Sem r (Maybe Response)
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
  runFinal $ asyncToIOFinal $ embedToFinal $ traceToIO runServer
