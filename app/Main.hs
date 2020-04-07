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



consumeExactly :: ByteBuffer -> TVar Int -> TVar Bool -> Int -> IO (Maybe ByteString)
consumeExactly buffer var term count = do
  putStrLn $ "Wanting to consume exactly " <> show count <> " bytes, waiting for that"
  res <- atomically $ do
    termVal <- readTVar term
    if termVal then
      return False
    else do
      available <- readTVar var
      check (available >= count)
      modifyTVar var (subtract count)
      return True
  if res then do
    putStrLn $ "Got " <> show count <> " bytes, getting and returning them"
    Right bytes <- consume buffer count
    return (Just bytes)
  else return Nothing

readPacket :: ByteBuffer -> TVar Int -> TVar Bool -> IO (Maybe ByteString)
readPacket buffer var term = do
  putStrLn "Trying to read the size"
  getSize mempty >>= \case
    Nothing -> return Nothing
    Just (VarInt size) -> do
      putStrLn $ "Size is " <> show size <> ", trying to get that many bytes"
      consumeExactly buffer var term (fromIntegral size)
  where
    getSize :: ByteString -> IO (Maybe VarInt)
    getSize previous = do
      putStrLn "For getting the size, reading the next byte"
      consumeExactly buffer var term 1 >>= \case
        Nothing -> return Nothing
        Just onebyte -> do
          let bytes = previous <> onebyte
          putStrLn "For getting the size, try to decode with the next byte appended"
          case decode bytes of
            Left exception -> do
              putStrLn $ "For getting the size, not yet enough bytes, probably: " <> show exception
              getSize bytes
            Right count -> return (Just count)

statusPacket :: ServerPacket StatusState
statusPacket = ServerPacketResponse response where
  response = Response
      { response_version = ResponseVersion "1.15.2" 578
      , response_players = ResponsePlayers 100000000 100000001 []
      , response_description = ResponseDescription "NANI SORE"
      }


encodePacket :: Store (ServerPacket s) => ServerPacket s -> ByteString
encodePacket packet =
  let payload = encode packet
      header = encode (VarInt (fromIntegral (BS.length payload)))
  in header <> payload

clientHandler :: Socket -> IO ()
clientHandler socket = with Nothing $ \buffer -> do
  var <- newTVarIO 0
  term <- newTVarIO False
  forkIO $ receiveLoop socket buffer var term
  run buffer var term
  close socket
  where
    run :: ByteBuffer -> TVar Int -> TVar Bool -> IO ()
    run buffer var term = getNext @HandshakingState $ \case
      ClientPacketHandshake handshake -> do
        putStrLn $ "Got handshake: " <> show handshake
        case nextState handshake of
          VarInt 1 -> do
            putStrLn "Next state is status"
            clientLoopStatus
          VarInt 2 -> do
            putStrLn "Next state is login"
            clientLoopLogin
      where
        getNext :: forall s . Store (ClientPacket s) => (ClientPacket s -> IO ()) -> IO ()
        getNext handler = readPacket buffer var term >>= \case
          Nothing -> putStrLn "Exiting without getting any messages"
          Just bytes -> case decode bytes :: Either PeekException (ClientPacket s) of
            Left exc -> print exc
            Right value -> handler value


        clientLoopStatus :: IO ()
        clientLoopStatus = getNext @StatusState $ \case
          ClientPacketRequest -> do
            putStrLn "Got request!"
            sendAll socket (encodePacket statusPacket)
            clientLoopStatus
          ClientPacketPing nonce -> do
            putStrLn "Got a ping!"
            sendAll socket (encodePacket (ServerPacketPong nonce))
            clientLoopStatus

        clientLoopLogin :: IO ()
        clientLoopLogin = getNext @LoginState $ \case
          _ -> undefined


receiveLoop :: Socket -> ByteBuffer -> TVar Int -> TVar Bool -> IO ()
receiveLoop socket buffer var term = do
  putStrLn "Trying to receive up to 4096 bytes"
  bytes <- recv socket 4096
  if BS.null bytes then do
    atomically $ writeTVar term True
    putStrLn "Seems like no more bytes could be received"
  else do
    putStrLn $ "Was able to receive " <> show (BS.length bytes) <> " bytes, copying to buffer"
    copyByteString buffer bytes
    atomically $ modifyTVar' var (+ BS.length bytes)
    receiveLoop socket buffer var term


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  socketRunner clientHandler
