{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Minecraft where

import Data.Store
import Data.Store.Internal (combineSize, combineSizeWith, getSizeWith, getSize)
import Data.Int (Int32, Int64)
import Data.Bits
import Data.Word (Word8, Word32, Word16)
import Data.Proxy
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.List (groupBy)
import Data.Text.Encoding
import Data.Foldable (traverse_)
import Control.Monad (replicateM)

import Control.Applicative
import Data.Void (absurd)
import Data.Functor.Contravariant.Divisible
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS

import GHC.TypeLits
-- See https://wiki.vg/Protocol#VarInt_and_VarLong
newtype VarInt = VarInt Int32 deriving (Show, Eq)


instance Store VarInt where
  size = VarSize (\(VarInt value) -> case value of
                     0 -> 1
                     _ -> (32 - countLeadingZeros value + 6) `div` 7)
  poke (VarInt value) = loop value where
    loop :: Int32 -> Poke ()
    loop value = if left == 0 then poke right else poke (setBit right 7) >> loop left where
      right :: Word8 = fromIntegral value .&. 0b01111111
      left :: Int32 = fromIntegral (fromIntegral value `shiftR` 7 :: Word32)
  peek = VarInt <$> loop where
    loop :: Peek Int32
    loop = do
      r :: Word8 <- peek
      rest <- if testBit r 7 then loop else pure 0
      pure $ rest `shiftL` 7 .|. fromIntegral (clearBit r 7)


newtype MCString = MCString Text deriving (Show, Eq)

instance Store MCString where
  size = VarSize f where
    f (MCString text) = lengthSize + BS.length encoded where
      encoded = encodeUtf8 text
      VarSize lengthSizeFun = size :: Size VarInt
      lengthSize = lengthSizeFun $ VarInt (fromIntegral (BS.length encoded))

  poke (MCString text) = poke (VarInt (fromIntegral (BS.length encoded))) >> traverse_ poke (BS.unpack encoded) where
    encoded = encodeUtf8 text
  peek = do
    VarInt len <- peek :: Peek VarInt
    bytes :: [Word8] <- replicateM (fromIntegral len) peek
    pure $ MCString (decodeUtf8 (BS.pack bytes))

data Handshake = Handshake
  { protocolVersion :: Int32
  , serverAddress :: Text
  , serverPort :: Word16
  , nextState :: ServerState
  } deriving (Generic, Show, Eq)

instance Store Handshake where
  size = VarSize f where
    f Handshake { .. } = getSize (VarInt protocolVersion) + getSize (MCString serverAddress) + getSize serverPort + getSize (VarInt (fromIntegral (fromEnum nextState)))
  poke Handshake { .. } = poke (VarInt protocolVersion) >> poke (MCString serverAddress) >> poke serverPort >> poke (VarInt (fromIntegral (fromEnum nextState)))
  peek = do
    VarInt protocolVersion <- peek
    MCString serverAddress <- peek
    serverPort <- peek
    VarInt nextStateInt <- peek
    let nextState = toEnum (fromIntegral nextStateInt)
    return Handshake { .. }

data ServerState = HandshakingState | StatusState | LoginState deriving (Show, Eq, Enum)

data ClientPacket (s :: ServerState) where
  ClientPacketHandshake :: Handshake -> ClientPacket HandshakingState
  ClientPacketRequest :: ClientPacket StatusState
  ClientPacketPing :: Int64 -> ClientPacket StatusState

instance Store (ClientPacket HandshakingState) where
  size = VarSize f where
    f :: ClientPacket HandshakingState -> Int
    f (ClientPacketHandshake handshake) = getSize (VarInt 0) + getSize handshake
  poke (ClientPacketHandshake handshake) = poke (VarInt 0) >> poke handshake
  peek = peek >>= \case
    VarInt 0 -> ClientPacketHandshake <$> peek

instance Store (ClientPacket StatusState) where
  size = VarSize f where
    f :: ClientPacket StatusState -> Int
    f ClientPacketRequest = getSize (VarInt 0)
    f (ClientPacketPing value) = getSize (VarInt 1) + getSize value
  poke ClientPacketRequest = poke (VarInt 0)
  poke (ClientPacketPing value) = poke (VarInt 1) >> poke value
  peek = peek >>= \case
    VarInt 0 -> pure ClientPacketRequest
    VarInt 1 -> ClientPacketPing <$> peek

instance Store (ClientPacket LoginState) where
  size = undefined
  poke = undefined
  peek = undefined

data ServerPacket (s :: ServerState) where
  ServerPacketResponse :: Response -> ServerPacket StatusState
  ServerPacketPong :: Int64 -> ServerPacket StatusState

instance Store (ServerPacket StatusState) where
  size = VarSize f where
    f :: ServerPacket StatusState -> Int
    f (ServerPacketResponse response) = getSize (VarInt 0) + getSize (responseToMCString response)
    f (ServerPacketPong value) = getSize (VarInt 1) + getSize value
  poke (ServerPacketResponse response) = poke (VarInt 0) >> poke (responseToMCString response)
  poke (ServerPacketPong value) = poke (VarInt 1) >> poke value
  peek = peek >>= \case
    VarInt 0 -> do
      Just response <- mcStringToResponse <$> peek
      return (ServerPacketResponse response)
    VarInt 1 -> ServerPacketPong <$> peek

encodePacket :: Store (ServerPacket s) => ServerPacket s -> BS.ByteString
encodePacket packet =
  let payload = encode packet
      header = encode (VarInt (fromIntegral (BS.length payload)))
  in header <> payload

responseToMCString :: Response -> MCString
responseToMCString = MCString . decodeUtf8 . LBS.toStrict . A.encode

mcStringToResponse :: MCString -> Maybe Response
mcStringToResponse (MCString text) = A.decodeStrict (encodeUtf8 text)

jsonOptions :: A.Options
jsonOptions = A.defaultOptions
  { A.fieldLabelModifier = last . groupBy (\a b -> a /= '_' && b /= '_')
  }

data ResponseVersion = ResponseVersion
  { response_version_name :: Text
  , response_version_protocol :: Int
  } deriving (Generic, Show)

instance A.ToJSON ResponseVersion where
  toEncoding = A.genericToEncoding jsonOptions

instance A.FromJSON ResponseVersion where
  parseJSON = A.genericParseJSON jsonOptions

data ResponsePlayers = ResponsePlayers
  { response_players_max :: Int
  , response_players_online :: Int
  , response_players_sample :: [()]
  } deriving (Generic, Show)

instance A.ToJSON ResponsePlayers where
  toEncoding = A.genericToEncoding jsonOptions

instance A.FromJSON ResponsePlayers where
  parseJSON = A.genericParseJSON jsonOptions

newtype ResponseDescription = ResponseDescription
  { response_description_text :: Text
  } deriving (Generic, Show)

instance A.ToJSON ResponseDescription where
  toEncoding = A.genericToEncoding jsonOptions

instance A.FromJSON ResponseDescription where
  parseJSON = A.genericParseJSON jsonOptions

data Response = Response
  { response_version :: ResponseVersion
  , response_players :: ResponsePlayers
  , response_description :: ResponseDescription
  } deriving (Generic, Show)

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding jsonOptions

instance A.FromJSON Response where
  parseJSON = A.genericParseJSON jsonOptions
