{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module Connection where

import Config

import Data.Maybe (fromMaybe)
import Minecraft.Types
import Minecraft.Protocol
import Minecraft.Effect
import Polysemy
import Polysemy.Error
import Polysemy.Async
import Polysemy.Trace
import Polysemy.Reader
import Network.Socket
import Network.Socket.ByteString
import Control.Monad
import Control.Exception (IOException)
import qualified Control.Concurrent.Async as A
import qualified Data.ByteString as BS

getUpstreamAddr :: Members '[Error String, Embed IO] r => String -> Sem r AddrInfo
getUpstreamAddr ip = do
  let hints = defaultHints { addrSocketType = Stream }
  addresses <- embed $ getAddrInfo (Just hints) (Just ip) (Just "25565")
  case addresses of
    [] -> throw $ "Couldn't get any upstream addresses for ip " <> show ip
    address:_ -> return address

connectUpstream :: Members '[Error String, Embed IO] r => String -> Sem r Socket
connectUpstream ip = do
  addr <- getUpstreamAddr ip
  upstreamSocket <- embed $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  fromExceptionVia @IOException show $ connect upstreamSocket $ addrAddress addr
  return upstreamSocket


initListenSocket :: Members [Reader Config, Embed IO] r => Sem r Socket
initListenSocket = do
  port' <- asks (fromMaybe 25565 . port)
  embed $ do
    let hints = defaultHints
          { addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
          }
    -- TODO: Listen on all addresses to support both IPv4 and IPv6?
    addr <- addrAddress . head <$> getAddrInfo (Just hints) Nothing (Just (show port'))

    serverSocket <- socket AF_INET Stream defaultProtocol
    bind serverSocket addr
    listen serverSocket 16
    return serverSocket

isServerRunning :: Members '[Error String, Async, Trace, Final IO, Embed IO] r => String -> Sem r Bool
isServerRunning ip = do
  trace $ "Trying to determine whether " <> ip <> " has a running minecraft server"
  result <- try $ do
    trace "Connecting to the ip"
    upstreamSocket <- connectUpstream ip
    trace "Running a minecraft client"
    response <- runMinecraft upstreamSocket queryStatus
    trace $ "Got response: " <> show response
    return True
  case result of
    Left err -> do
      trace $ "Error while trying to determine whether " <> ip <> " is up: " <> show err
      return False
    Right result -> return result


runRelay :: forall r . Members '[Error String, Embed IO] r => Socket -> String -> Sem r ()
runRelay peerSocket ip = do
  upstreamSocket <- connectUpstream ip
  tcpRelay peerSocket upstreamSocket
  where
    tcpRelay :: Socket -> Socket -> Sem r ()
    tcpRelay a b = embed $ do
      void $ A.race (relay a b) (relay b a)
      close a
      close b

    relay :: Socket -> Socket -> IO ()
    relay from to = loop where
      loop = do
        bytes <- recv from 4096
        unless (BS.null bytes) $ do
          sendAll to bytes
          loop
