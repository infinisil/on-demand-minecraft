{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Minecraft.Effect where

import Minecraft.Types
import Minecraft.Packet

import Network.Socket
import Data.Store
import Control.Exception (IOException)

import Polysemy
import Polysemy.Async
import Polysemy.Trace
import Polysemy.Error
import qualified Polysemy.Internal
import qualified Polysemy.Internal.CustomErrors



data Side = Server | Client

type family Produces p where
  Produces Server = ServerPacket
  Produces Client = ClientPacket

type family Consumes p where
  Consumes Server = ClientPacket
  Consumes Client = ServerPacket

data Minecraft (side :: Side) (m :: * -> *) a where
  MaybeReceivePacket ::
    (packet ~ Consumes side state, Store packet, Show packet)
    => Minecraft side m (Maybe packet)
  ReceivePacket ::
    (packet ~ Consumes side state, Store packet, Show packet)
    => Minecraft side m (Consumes side state)
  SendPacket ::
    (packet ~ Produces side state, Store packet, Show packet)
    => packet -> Minecraft side m ()


-- These are the same as with
--   makeSem ''Minecraft
-- But with the state argument moved to the front for more convenience

type instance Polysemy.Internal.CustomErrors.DefiningModule Minecraft = "Minecraft.Packet"

{-# INLINABLE maybeReceivePacket #-}
maybeReceivePacket :: forall state side r packet
  . (MemberWithError (Minecraft side) r, packet ~ Consumes side state, Store packet, Show packet)
  => Sem r (Maybe packet)
maybeReceivePacket = Polysemy.Internal.send MaybeReceivePacket

{-# INLINABLE receivePacket #-}
receivePacket :: forall state side r packet
  . (MemberWithError (Minecraft side) r, packet ~ Consumes side state, Store packet, Show packet)
  => Sem r (Consumes side state)
receivePacket = Polysemy.Internal.send ReceivePacket

{-# INLINABLE sendPacket #-}
sendPacket :: forall state side r packet
  . (MemberWithError (Minecraft side) r, packet ~ Produces side state, Store packet, Show packet)
  => packet -> Sem r ()
sendPacket = Polysemy.Internal.send . SendPacket


runMinecraft :: forall side r a . Members '[Trace, Async, Final IO, Error String] r => Socket -> Sem (Minecraft side ': r) a -> Sem r a
runMinecraft socket action = runByteBufferAction $ runSocketAction socket $ do
  result <- reinterpret2 interpreter action
  socketClose
  return result
  where
    interpreter :: forall m x . Minecraft side m x -> Sem (SocketAction ': ByteBufferAction ': r) x
    interpreter (SendPacket packet) = writePacket packet
    interpreter MaybeReceivePacket = readPacket >>= \case
        Nothing -> trace "Client closed connection" >> return Nothing
        Just packet -> return $ Just packet
    interpreter ReceivePacket = readPacket >>= \case
        Nothing -> throw "Client closed connection while a packet was expected"
        Just packet -> return packet
