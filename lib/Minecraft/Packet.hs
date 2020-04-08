{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Minecraft.Packet where

import Minecraft.Types

import Network.Socket
import Network.Socket.ByteString
import System.IO.ByteBuffer
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Store

import Polysemy
import Polysemy.Resource
import Polysemy.Async
import Polysemy.Trace
import Polysemy.Error


data ByteBufferAction (m :: * -> *) a where
  ByteBufferGet :: Int -> ByteBufferAction m (Maybe ByteString)
  ByteBufferPut :: ByteString -> ByteBufferAction m ()
  ByteBufferDone :: ByteBufferAction m ()

makeSem ''ByteBufferAction

runByteBufferAction :: forall r a . Member (Final IO) r => Sem (ByteBufferAction ': r) a -> Sem r a
runByteBufferAction action = resourceToIOFinal $ bracket (embedFinal $ new Nothing) (embedFinal . free) $ \buf -> raise $ do
  byteCount :: TVar Int <- embedFinal $ atomically (newTVar 0)
  isDone <- embedFinal $ atomically (newTVar False)

  let interpreter :: forall m x . ByteBufferAction m x -> Sem r x
      interpreter ByteBufferDone =
        embedFinal $ atomically $ writeTVar isDone True
      interpreter (ByteBufferPut bytes) = do
        embedFinal $ copyByteString buf bytes
        embedFinal $ atomically $ modifyTVar' byteCount (+ BS.length bytes)
      interpreter (ByteBufferGet count) = do
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

  interpret interpreter action


data SocketAction (m :: * -> *) a where
  SocketSend :: ByteString -> SocketAction m ()
  SocketRecv :: Int -> SocketAction m (Maybe ByteString)
  SocketClose :: SocketAction m ()

makeSem ''SocketAction

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

  receiveLoop :: Sem (SocketAction ': r) ()
  receiveLoop = socketRecv 4096 >>= \case
    Nothing -> byteBufferDone
    Just bytes -> do
      byteBufferPut bytes
      receiveLoop



readPacket :: forall r s packet . (Store packet, Show packet, Members '[Trace, ByteBufferAction, SocketAction, Error String] r) => Sem r (Maybe packet)
readPacket = nextBytes >>= \case
  Nothing -> return Nothing
  Just bytes -> case decode bytes of
    Left exc -> throw $ "Couldn't decode packet: " <> show exc
    Right packet -> do
      trace $ "Received packet: " <> show packet
      return $ Just packet
  where
    nextBytes :: Sem r (Maybe ByteString)
    nextBytes = getSize mempty >>= \case
      Nothing -> return Nothing
      Just size -> byteBufferGet size

    getSize :: ByteString -> Sem r (Maybe Int)
    getSize previous = byteBufferGet 1 >>= \case
      Nothing -> return Nothing
      Just onebyte ->
        let bytes = previous <> onebyte
        in case decode bytes of
          Left _ -> getSize bytes
          Right (MCVarInt count) -> return $ Just (fromIntegral count)


writePacket :: (Store packet, Show packet, Members '[Trace, SocketAction] r) => packet -> Sem r ()
writePacket packet = do
  trace $ "Sending packet: " <> show packet
  socketSend (header <> payload)
  where
    payload = encode packet
    header = encode (MCVarInt (fromIntegral (BS.length payload)))
