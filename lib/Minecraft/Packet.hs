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

import Polysemy
import Polysemy.Resource
import Polysemy.Async
import Polysemy.Trace

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

runSocketAction :: Member (Final IO) r => Socket -> Sem (SocketAction ': r) a -> Sem r a
runSocketAction socket = interpret $ \case
  SocketSend bytes -> embedFinal $ sendAll socket bytes
  SocketRecv count -> do
    bytes <- embedFinal $ recv socket count
    return $ if BS.null bytes
      then Nothing
      else Just bytes
  SocketClose -> embedFinal $ close socket

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

type PacketSender h r = forall (s :: ServerState) p . (p ~ h s, Show p, Store p) => p -> Sem r ()
type PacketReceiver h r = forall (s :: ServerState) p a . (p ~ h s, Show p, Store p) => (p -> Sem r a) -> Sem r (Maybe a)

serverProtocolRunner
  :: forall a r
   . Members '[Trace, ByteBufferAction, Async, SocketAction] r
  => ( PacketSender ServerPacket r -> PacketReceiver ClientPacket r -> Sem r a )
  -> Sem r a
serverProtocolRunner = protocolRunner

clientProtocolRunner
  :: forall a r
   . Members '[Trace, ByteBufferAction, Async, SocketAction] r
  => ( PacketSender ClientPacket r -> PacketReceiver ServerPacket r -> Sem r a )
  -> Sem r a
clientProtocolRunner = protocolRunner

protocolRunner
  :: forall i o a r
   . Members '[Trace, ByteBufferAction, Async, SocketAction] r
  => ( PacketSender o r -> PacketReceiver i r -> Sem r a )
  -> Sem r a
protocolRunner handler = do
  async $ receiveLoop
  result <- handler sendPacket receivePacket
  socketClose
  return result


sendPacket :: forall h (s :: ServerState) p r . (Members '[Trace, SocketAction] r, p ~ h s, Show p, Store p) => p -> Sem r ()
sendPacket packet = do
  trace $ "Sending packet: " <> show packet
  let payload = encode packet
      header = encode (MCVarInt (fromIntegral (BS.length payload)))
  socketSend (header <> payload)

receiveLoop :: Members '[ByteBufferAction, SocketAction] r => Sem r ()
receiveLoop = socketRecv 4096 >>= \case
  Nothing -> byteBufferDone
  Just bytes -> do
    byteBufferPut bytes
    receiveLoop

receivePacket :: forall h (s :: ServerState) p a r . (Members '[ByteBufferAction, Trace] r, p ~ h s, Show p, Store p) => (p -> Sem r a) -> Sem r (Maybe a)
receivePacket handler = nextBytes >>= \case
  Nothing -> trace "Client closed connection" >> return Nothing
  Just bytes -> case decode bytes :: Either PeekException p of
    Left exc -> trace (show exc) >> return Nothing
    Right packet -> do
      trace $ "Received packet: " <> show packet
      result <- handler packet
      return $ Just result

  where

  nextBytes :: Sem r (Maybe ByteString)
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
