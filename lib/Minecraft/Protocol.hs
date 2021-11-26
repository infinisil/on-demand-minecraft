{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Minecraft.Protocol where

import Minecraft.Types
import Minecraft.Effect
import Polysemy
import Polysemy.Trace
import Polysemy.Reader
import Data.Text (Text, unpack)

queryStatus :: Members '[Trace, Minecraft Client] r => Sem r Response
queryStatus = do
  sendPacket $ ClientPacketHandshake $ Handshake
    { protocolVersion = 756
    , serverAddress = ""
    , serverPort = 25565
    , nextState = StatusState
    }
  sendPacket ClientPacketRequest
  receivePacket @StatusState >>= \case
    ServerPacketResponse response -> return response

type WhitelistCheck r = Text -> Sem r (Maybe Text)
type StatusResponse = Text
type LoginResponse r = Sem r Text

shallowServer :: forall r . Members '[Trace, Minecraft Server] r => WhitelistCheck r -> StatusResponse -> LoginResponse r -> Sem r ()
shallowServer whitelistCheck statusResponse loginResponse = receivePacket @HandshakingState >>= \case
  ClientPacketHandshake handshake -> case nextState handshake of
    StatusState -> clientLoopStatus
    LoginState -> clientLoopLogin

  where
    clientLoopStatus :: Sem r ()
    clientLoopStatus = maybeReceivePacket @StatusState >>= \case
      Nothing -> trace "Client disconnected"
      Just ClientPacketRequest -> do
        sendPacket $ ServerPacketResponse $ Response
          { response_version = ResponseVersion "1.17.1" 756
          , response_players = ResponsePlayers (-1) 0
          , response_description = Chat statusResponse
          }
        clientLoopStatus
      Just (ClientPacketPing nonce) -> do
        sendPacket $ ServerPacketPong nonce
        clientLoopStatus

    clientLoopLogin :: Sem r ()
    clientLoopLogin = receivePacket @LoginState >>= \case
      ClientPacketLoginStart name -> whitelistCheck name >>= \case
        Nothing -> do
          trace $ "Non-whitelisted player attempted to join: " <> unpack name
          sendPacket $ ServerPacketDisconnect "You're not whitelisted"
        Just uuid -> do
          message <- loginResponse
          sendPacket $ ServerPacketDisconnect message
