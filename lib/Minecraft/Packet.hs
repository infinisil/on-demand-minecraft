{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Minecraft.Packet where

import Minecraft.Types

import Network.Socket
import Network.Socket.ByteString
import System.IO.ByteBuffer
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Store
import qualified Polysemy.Internal
import qualified Polysemy.Internal.CustomErrors

import Polysemy
import Polysemy.Resource
import Polysemy.Async
import Polysemy.Trace
import Polysemy.Error

data SocketAction (m :: * -> *) a where
  SocketSend :: ByteString -> SocketAction m ()
  SocketRecv :: Int -> SocketAction m (Maybe ByteString)
  SocketClose :: SocketAction m ()

data ByteBufferAction (m :: * -> *) a where
  ByteBufferGet :: Int -> ByteBufferAction m (Maybe ByteString)
  ByteBufferPut :: ByteString -> ByteBufferAction m ()
  ByteBufferDone :: ByteBufferAction m ()


makeSem ''SocketAction
makeSem ''ByteBufferAction

runSocketAction :: forall r a . Members '[Async, Final IO, ByteBufferAction] r => Socket -> Sem (SocketAction ': r) a -> Sem r a
runSocketAction socket action = interpret interpreter (async receiveLoop >> action) where
  interpreter :: forall m x . SocketAction m x -> Sem r x
  interpreter (SocketSend bytes) = embedFinal $ sendAll socket bytes
  interpreter (SocketRecv count) = do
    bytes <- embedFinal $ recv socket count
    return $ if BS.null bytes
      then Nothing
      else Just bytes
  interpreter SocketClose = embedFinal $ close socket

runByteBufferAction :: Member (Final IO) r => Sem (ByteBufferAction ': r) a -> Sem r a
runByteBufferAction action = resourceToIOFinal
  $ bracket
    (embedFinal $ new Nothing)
    (embedFinal . free)
    \buf -> raise $ do
      byteCount :: TVar Int <- embedFinal $ atomically (newTVar 0)
      isDone <- embedFinal $ atomically (newTVar False)
      interpret \case
          ByteBufferDone -> do
            embedFinal $ atomically $ writeTVar isDone True
          ByteBufferPut bytes -> do
            embedFinal $ copyByteString buf bytes
            embedFinal $ atomically $ modifyTVar' byteCount (+ BS.length bytes)
          ByteBufferGet count -> do
            hasEnough <- embedFinal $ atomically $ do
              available <- readTVar byteCount
              termVal <- readTVar isDone
              case (available >= count, termVal) of
                -- If we have enough bytes, subtract them from the counter and return success,
                -- doesn't matter if the connection is already terminated
                (True, _) -> do
                  modifyTVar' byteCount (subtract count)
                  return True
                -- However if we can't get enough bytes and the connection is terminated, we can't ever get more, return failure
                (False, True) -> return False
                -- If we can't get enough bytes but the connection is still going, wait until something has changed and evaluate again
                (False, False) -> retry

            if hasEnough then do
              -- We know we have enough bytes already, so we can ignore the Left case
              result <- embedFinal $ consume buf count
              case result of
                Right bytes -> return $ Just bytes
            else return Nothing
        $ action

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


-- The same as with
--   makeSem ''Minecraft
-- But with the state argument moved to the front for more convenience
type instance Polysemy.Internal.CustomErrors.DefiningModule Minecraft = "Minecraft.Packet"
maybeReceivePacket ::
  forall state side r packet.
  (MemberWithError (Minecraft side) r,
    (~) packet (Consumes side state),
    Store packet,
    Show packet) =>
  Sem r (Maybe packet)
{-# INLINABLE maybeReceivePacket #-}
maybeReceivePacket
  = Polysemy.Internal.send
      (MaybeReceivePacket ::
          Minecraft side (Sem r) (Maybe packet))
receivePacket ::
  forall state side r packet.
  (MemberWithError (Minecraft side) r,
    (~) packet (Consumes side state),
    Store packet,
    Show packet) =>
  Sem r (Consumes side state)
{-# INLINABLE receivePacket #-}
receivePacket
  = Polysemy.Internal.send
      (ReceivePacket ::
          Minecraft side (Sem r) (Consumes side state))
sendPacket ::
  forall state side r packet.
  (MemberWithError (Minecraft side) r,
    (~) packet (Produces side state),
    Store packet,
    Show packet) =>
  packet -> Sem r ()
{-# INLINABLE sendPacket #-}
sendPacket x
  = Polysemy.Internal.send
      (SendPacket x :: Minecraft side (Sem r) ())


runMinecraft :: forall side r a . Members '[Trace, Async, Final IO, Error String] r => Socket -> Sem (Minecraft side ': r) a -> Sem r a
runMinecraft socket action = runByteBufferAction $ runSocketAction socket $ do
  async receiveLoop
  result <- reinterpret2 interpreter action
  socketClose
  return result
  where
    interpreter :: forall m x . Minecraft side m x -> Sem (SocketAction ': ByteBufferAction ': r) x
    interpreter (SendPacket packet) = do
        trace $ "Sending packet: " <> show packet
        let payload = encode packet
            header = encode (MCVarInt (fromIntegral (BS.length payload)))
        socketSend (header <> payload)
    interpreter MaybeReceivePacket = maybeReceive >>= \case
        Nothing -> trace "Client closed connection" >> return Nothing
        Just packet -> return $ Just packet
    interpreter ReceivePacket = maybeReceive >>= \case
        Nothing -> throw "Client closed connection while a packet was expected"
        Just packet -> return packet



    maybeReceive :: forall r s packet . (Store packet, Show packet, Members '[Trace, ByteBufferAction, SocketAction, Error String] r) => Sem r (Maybe packet)
    maybeReceive = nextBytes >>= \case
      Nothing -> return Nothing
      Just bytes -> case decode bytes of
        Left exc -> throw $ "Couldn't decode packet: " <> show exc
        Right packet -> do
          trace $ "Received packet: " <> show packet
          return $ Just packet

    nextBytes :: forall r . Member ByteBufferAction r => Sem r (Maybe ByteString)
    nextBytes = getSize mempty >>= \case
      Nothing -> return Nothing
      Just (MCVarInt size) -> byteBufferGet (fromIntegral size)
      where
        getSize :: ByteString -> Sem r (Maybe MCVarInt)
        getSize previous = byteBufferGet 1 >>= \case
          Nothing -> return Nothing
          Just onebyte ->
            let bytes = previous <> onebyte
            in case decode bytes of
              Left _ -> getSize bytes
              Right count -> return (Just count)

receiveLoop :: Members '[ByteBufferAction, SocketAction] r => Sem r ()
receiveLoop = socketRecv 4096 >>= \case
  Nothing -> byteBufferDone
  Just bytes -> do
    byteBufferPut bytes
    receiveLoop
