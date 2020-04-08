{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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


type PacketSender h = forall (s :: ServerState) p . (p ~ h s, Show p, Store p) => p -> IO ()
type PacketReceiver h = forall (s :: ServerState) p a . (p ~ h s, Show p, Store p) => (p -> IO a) -> IO (Maybe a)

serverProtocolRunner
  :: Socket
  -> ( PacketSender ServerPacket -> PacketReceiver ClientPacket -> IO a )
  -> IO a
serverProtocolRunner = protocolRunner

clientProtocolRunner
  :: Socket
  -> ( PacketSender ClientPacket -> PacketReceiver ServerPacket -> IO a )
  -> IO a
clientProtocolRunner = protocolRunner

protocolRunner
  :: forall i o a . Socket
  -> ( PacketSender o -> PacketReceiver i -> IO a )
  -> IO a
protocolRunner socket handler = with Nothing $ \buf -> do
  bufferEnv <- BufferEnv buf <$> newTVarIO 0 <*> newTVarIO False
  forkIO $ receiveLoop socket bufferEnv
  result <- handler (sendPacket socket) (receivePacket bufferEnv)
  close socket
  return result


sendPacket :: forall h (s :: ServerState) p . (p ~ h s, Show p, Store p) => Socket -> p -> IO ()
sendPacket socket packet = do
  putStrLn $ "Sending packet: " <> show packet
  let payload = encode packet
      header = encode (MCVarInt (fromIntegral (BS.length payload)))
  sendAll socket (header <> payload)

data BufferEnv = BufferEnv
  { byteBuffer :: ByteBuffer
  , byteCount :: TVar Int
  , isDone :: TVar Bool
  }


receiveLoop :: Socket -> BufferEnv -> IO ()
receiveLoop socket bufferEnv@BufferEnv { .. } = do
  bytes <- recv socket 4096
  if BS.null bytes then
    atomically $ writeTVar isDone True
  else do
    copyByteString byteBuffer bytes
    atomically $ modifyTVar' byteCount (+ BS.length bytes)
    receiveLoop socket bufferEnv


consumeExactly :: BufferEnv -> Int -> IO (Maybe ByteString)
consumeExactly BufferEnv { .. } count = do
  -- Atomically determine whether we have as many bytes as we need or if the connection closed before that
  -- This blocks if not enough bytes have been read to determine this
  hasEnough <- atomically $ do
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
    Right bytes <- consume byteBuffer count
    return $ Just bytes
  else return Nothing

receivePacket :: forall h (s :: ServerState) p a . (p ~ h s, Show p, Store p) => BufferEnv -> (p -> IO a) -> IO (Maybe a)
receivePacket bufferEnv handler = nextBytes >>= \case
  Nothing -> putStrLn "Client closed connection" >> return Nothing
  Just bytes -> case decode bytes :: Either PeekException p of
    Left exc -> print exc >> return Nothing
    Right packet -> do
      putStrLn $ "Received packet: " <> show packet
      result <- handler packet
      return $ Just result

  where

  nextBytes :: IO (Maybe ByteString)
  nextBytes = getSize mempty >>= \case
    Nothing -> return Nothing
    Just (MCVarInt size) -> consumeExactly bufferEnv (fromIntegral size)
    where
      getSize :: ByteString -> IO (Maybe MCVarInt)
      getSize previous = consumeExactly bufferEnv 1 >>= \case
        Nothing -> return Nothing
        Just onebyte ->
          let bytes = previous <> onebyte
          in case decode bytes of
            Left _ -> getSize bytes
            Right count -> return (Just count)
