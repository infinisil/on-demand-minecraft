module Main where

import Network.Socket
import Network.Socket.Activation (getActivatedSockets)

import Control.Concurrent (forkIO)
import System.IO

import Data.Functor (void)
import Control.Monad (forever)

handleClient :: Handle -> IO ()
handleClient handle = do
  isEOF <- hIsEOF handle
  if isEOF then hClose handle
  else do
    line <- hGetLine handle
    if line == "quit" then do
      hPutStrLn handle "See ya"
      hClose handle
    else do
      hPutStrLn handle line
      handleClient handle

-- TODO: Exit when all clients have been handled, such that the systemd service can exit
acceptLoop :: Socket -> IO ()
acceptLoop serverSocket = forever $ do
  (socket, addr) <- accept serverSocket
  putStrLn $ "Accepted connection from " <> show addr
  handle <- socketToHandle socket ReadWriteMode
  hSetBuffering handle LineBuffering
  void $ forkIO (handleClient handle)

main :: IO ()
main = do
  -- Such that log messages appear immediately in journald
  hSetBuffering stdout LineBuffering
  msockets <- getActivatedSockets
  case msockets of
    Just [socket] -> do
      -- For some reason this is needed to forkIO more than one connection
      withFdSocket socket setNonBlockIfNeeded
      putStrLn "Received a socket from systemd, now accepting connections on it"
      acceptLoop socket
    Just sockets -> putStrLn $ "Service was started with " <> show (length sockets) <> " sockets, but it should be 1"
    -- TODO: Make this be an alternative way to start the actual server
    Nothing -> putStrLn "Service wasn't started through socket activation, this is currently not supported"
