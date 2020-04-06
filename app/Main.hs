module Main where

import Socket

import Network.Socket
import Network.Socket.Activation (getActivatedSockets)

import Control.Concurrent (forkIO, killThread)
import System.IO

import Data.Functor (void)
import Control.Monad (forever)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

clientHandler :: Socket -> IO ()
clientHandler socket = do
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  socketRunner clientHandler
