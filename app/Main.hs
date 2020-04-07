{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Socket
import Network.Socket
import Network.Socket.ByteString
import System.IO

import System.IO.ByteBuffer
import Minecraft
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Store
import Data.Text.Encoding

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS

data BufferEnv = BufferEnv
  { byteBuffer :: ByteBuffer
  , byteCount :: TVar Int
  , isDone :: TVar Bool
  }

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


handleNextPacket :: forall s . Store (ClientPacket s) => BufferEnv -> (ClientPacket s -> IO ()) -> IO ()
handleNextPacket bufferEnv handler = nextBytes >>= \case
  Nothing -> putStrLn "Exiting without getting any messages"
  Just bytes -> case decode bytes :: Either PeekException (ClientPacket s) of
    Left exc -> print exc
    Right value -> handler value

  where

  nextBytes :: IO (Maybe ByteString)
  nextBytes = getSize mempty >>= \case
    Nothing -> return Nothing
    Just (VarInt size) -> consumeExactly bufferEnv (fromIntegral size)
    where
      getSize :: ByteString -> IO (Maybe VarInt)
      getSize previous = consumeExactly bufferEnv 1 >>= \case
        Nothing -> return Nothing
        Just onebyte ->
          let bytes = previous <> onebyte
          in case decode bytes of
            Left _ -> getSize bytes
            Right count -> return (Just count)

statusPacket :: ServerPacket StatusState
statusPacket = ServerPacketResponse $ Response
  { response_version = ResponseVersion "1.15.2" 578
  , response_players = ResponsePlayers 20 0 []
  , response_description = ResponseDescription "Server isn't running"
  }

server :: Socket -> IO ()
server socket = with Nothing $ \buf -> do
  bufferEnv <- BufferEnv buf <$> newTVarIO 0 <*> newTVarIO False
  forkIO $ receiveLoop socket bufferEnv
  run bufferEnv
  close socket
  where
    run :: BufferEnv -> IO ()
    run bufferEnv = handleNextPacket @HandshakingState bufferEnv $ \case
      ClientPacketHandshake handshake -> do
        putStrLn $ "Got handshake: " <> show handshake
        case nextState handshake of
          StatusState -> do
            putStrLn "Next state is status"
            clientLoopStatus
          LoginState -> do
            putStrLn "Next state is login"
            clientLoopLogin
      where
        clientLoopStatus :: IO ()
        clientLoopStatus = handleNextPacket @StatusState bufferEnv $ \case
          ClientPacketRequest -> do
            putStrLn "Got request!"
            sendAll socket (encodePacket statusPacket)
            clientLoopStatus
          ClientPacketPing nonce -> do
            putStrLn "Got a ping!"
            sendAll socket (encodePacket (ServerPacketPong nonce))
            clientLoopStatus

        clientLoopLogin :: IO ()
        clientLoopLogin = handleNextPacket @LoginState bufferEnv $ \case
          _ -> undefined


receiveLoop :: Socket -> BufferEnv -> IO ()
receiveLoop socket bufferEnv@BufferEnv { .. } = do
  bytes <- recv socket 4096
  if BS.null bytes then
    atomically $ writeTVar isDone True
  else do
    copyByteString byteBuffer bytes
    atomically $ modifyTVar' byteCount (+ BS.length bytes)
    receiveLoop socket bufferEnv


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  socketRunner server
