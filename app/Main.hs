{-# LANGUAGE LambdaCase          #-}
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

statusPacket :: ServerStatusPacket
statusPacket = ServerStatusResponse (MCString encoded) where
  response = Response
      { response_version = ResponseVersion "1.15.2" 578
      , response_players = ResponsePlayers 100000000 100000001 []
      , response_description = ResponseDescription "NANI SORE"
      }
  encoded = decodeUtf8 (LBS.toStrict (A.encode response))

clientHandler :: Socket -> IO ()
clientHandler socket = with Nothing $ \buffer -> do
  var <- newTVarIO 0
  term <- newTVarIO False
  forkIO $ receiveLoop socket buffer var term
  let loop state = readPacket buffer var term >>= \case
        Nothing -> putStrLn "Terminated"
        Just packet -> case state of
          (VarInt 0) -> case decode packet of
            Left exc -> print exc
            Right (HandshakePacket handshake) -> do
              putStrLn $ "Received handshake: " <> show handshake
              loop (nextState handshake)
          (VarInt 1) -> case decode packet of
            Left exc -> print exc
            Right ClientStatusRequest -> do
              putStrLn "Received request!"
              let bytes = encode statusPacket
                  allBytes = encode (VarInt (fromIntegral (BS.length bytes))) <> bytes
              sendAll socket allBytes
              loop state
            Right (ClientStatusPing val) -> do
              putStrLn $ "Received ping: " <> show val
              let bytes = encode (ServerStatusPong val)
                  allBytes = encode (VarInt (fromIntegral (BS.length bytes))) <> bytes
              sendAll socket allBytes
              loop state
          _ -> putStrLn $ "State " <> show state <> " not supported yet"
  loop (VarInt 0)
  gracefulClose socket 5000

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
