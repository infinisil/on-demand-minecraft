{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import Minecraft
import DigitalOcean
import Config
import State
import Connection

import Network.Socket (Socket, SockAddr, accept)
import Control.Monad (forever)
import Polysemy
import Polysemy.Async
import Polysemy.Trace
import Polysemy.Error
import Polysemy.Resource
import Polysemy.AtomicState
import Polysemy.Reader
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map

type MemberApp r = (Members
  '[ Error String
   , Trace
   , Async
   , Embed IO
   , Final IO
   , AtomicState UpstreamState
   ] r, MemberConfig r)

main :: IO ()
main = runFinal
  $ embedToFinal
  $ resourceToIOFinal
  $ bracket restoreState saveState $ \var -> runAtomicStateTVar var
  $ asyncToIOFinal
  $ traceToIO
  $ do
    result <- errorToIOFinal $ runConfig runServer
    case result of
      Left err -> trace err
      Right result -> return result

runServer :: MemberApp r => Sem r ()
runServer = do
  serverSocket <- initListenSocket
  trace "Now accepting connections"
  forever $ do
    peer <- embedFinal $ accept serverSocket
    async $ do
      result <- try $ handlePeer peer
      case result of
        Left err -> trace $ "Error during peer handling :" <> err
        Right result -> trace "Finished handling peer successfully"


handlePeer :: MemberApp r => (Socket, SockAddr) -> Sem r ()
handlePeer (peerSocket, peerAddr) = do
  trace $ "Accepted connection from " <> show peerAddr
  state <- updateAndGetState
  case state of
    Up (_, ip) -> do
      trace "Upstream is up, relaying connection"
      runRelay peerSocket ip
    Starting _ -> do
      trace "Upstream is not up, handling the connection locally"
      runMinecraft peerSocket
        $ shallowServer "Server starting.."
        $ whitelistPassing
        $ return "Server is already starting"
    Down -> do
      trace "Upstream is not up, handling the connection locally"
      runMinecraft peerSocket
        $ shallowServer "Server is down, join to start it"
        $ whitelistPassing
        $ do
          result <- runError $ runDigitalOcean startUpstream
          return $ case result of
            Left err -> Text.pack $ show err
            Right _ -> "Server now starting"
  where
    whitelistPassing :: Members '[Trace, Reader Whitelist] r => Sem r Text -> Text -> Sem r Text
    whitelistPassing action nick = do
      whitelisted <- elem nick <$> ask
      if whitelisted then action else do
        trace $ "Non-whitelisted player attempted to join: " <> Text.unpack nick
        return "You are not whitelisted"
