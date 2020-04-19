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
import System.IO
import Data.Time

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
  embed $ hSetBuffering stdout LineBuffering
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
    Up True _ ip -> do
      trace "Upstream is up, relaying connection"
      runRelay peerSocket ip
    Up False _ ip -> do
      trace "Upstream was up, but not anymore, handling the connection locally"
      runMinecraft peerSocket
        $ shallowServer whitelistPassing "Server temporarily down, probably maintenance happening"
        $ return "Server not up anymore"
    Starting _ since -> do
      trace "Upstream is not up, handling the connection locally"
      now <- embed getCurrentTime
      let duration = formatTime defaultTimeLocale "%ss" $ now `diffUTCTime` since
      runMinecraft peerSocket
        $ shallowServer whitelistPassing ("Server starting (" <> Text.pack duration <> ")..")
        $ return "Server is already starting"
    Down -> do
      trace "Upstream is not up, handling the connection locally"
      runMinecraft peerSocket
        $ shallowServer whitelistPassing "Server is down, join to start it" $ do
          result <- runError $ runDigitalOcean startUpstream
          return $ case result of
            Left err -> Text.pack $ show err
            Right _ -> "Server now starting"
  where
    whitelistPassing :: Member (Reader Whitelist) r => Text -> Sem r (Maybe Text)
    whitelistPassing nick = Map.lookup nick <$> ask
