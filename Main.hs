module Main where

import Network.Socket
import Network.Socket.Activation (getActivatedSockets)

import Control.Concurrent (forkIO, killThread)
import System.IO

import Data.Functor (void)
import Control.Monad (forever)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

handleClient :: TVar Int -> Socket -> IO ()
handleClient activeConnections socket = do
  handle <- socketToHandle socket ReadWriteMode
  hSetBuffering handle LineBuffering
  let loop = do
        isEOF <- hIsEOF handle
        if isEOF then hClose handle
        else do
          line <- hGetLine handle
          if line == "quit" then do
            hPutStrLn handle "See ya"
            hClose handle
          else do
            hPutStrLn handle line
            loop
  loop
  atomically (modifyTVar' activeConnections (subtract 1))


runServer :: TVar Int -> Socket -> IO ()
runServer activeConnections serverSocket = do
  (socket, addr) <- accept serverSocket
  atomically (modifyTVar' activeConnections (+1))
  putStrLn $ "Accepted connection from " <> show addr
  void $ forkIO (handleClient activeConnections socket)

main :: IO ()
main = do
  -- Such that log messages appear immediately in journald
  hSetBuffering stdout LineBuffering
  msockets <- getActivatedSockets
  case msockets of
    Just [serverSocket] -> do
      -- For some reason this is needed to forkIO more than one connection
      withFdSocket serverSocket setNonBlockIfNeeded
      activeConnections <- newTVarIO 0
      putStrLn "Received a socket from systemd, now accepting connections on it"
      runServer activeConnections serverSocket
      threadId <- forkIO $ forever $ runServer activeConnections serverSocket

      -- Wait until all accepted connections have been handled
      -- New connections are still accepted while others are being handled
      atomically $ do
        count <- readTVar activeConnections
        check (count == 0)
      killThread threadId
    Just sockets -> putStrLn $ "Service was started with " <> show (length sockets) <> " sockets, but it should be 1"
    -- TODO: Make this be an alternative way to start the actual server
    Nothing -> putStrLn "Service wasn't started through socket activation, this is currently not supported"
