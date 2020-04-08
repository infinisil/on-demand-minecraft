{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Minecraft where

import Data.Store
import Data.Store.Internal (getSize)
import Data.Int (Int32, Int64)
import Data.Word (Word8, Word16, Word32)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.List (groupBy)
import Data.Text.Encoding
import Data.Foldable (traverse_)
import Control.Monad (replicateM)
import qualified Data.Aeson as A

-- See https://wiki.vg/Protocol#VarInt_and_VarLong
newtype MCVarInt = MCVarInt Int32 deriving (Show, Eq)

instance Store MCVarInt where
  size = VarSize (\(MCVarInt value) -> case value of
                     0 -> 1
                     _ -> (32 - countLeadingZeros value + 6) `div` 7)
  poke (MCVarInt value) = loop value where
    loop :: Int32 -> Poke ()
    loop value = if left == 0 then poke right else poke (setBit right 7) >> loop left where
      right :: Word8 = fromIntegral value .&. 0b01111111
      left :: Int32 = fromIntegral (fromIntegral value `shiftR` 7 :: Word32)
  peek = MCVarInt <$> loop where
    loop :: Peek Int32
    loop = do
      r :: Word8 <- peek
      rest <- if testBit r 7 then loop else pure 0
      pure $ rest `shiftL` 7 .|. fromIntegral (clearBit r 7)

-- See https://wiki.vg/Protocol#Data_types
newtype MCString = MCString Text deriving (Show, Eq)

instance Store MCString where
  size = VarSize f where
    f (MCString text) = getSize (MCVarInt (fromIntegral (BS.length encoded))) + BS.length encoded where
      encoded = encodeUtf8 text
  poke (MCString text) = poke (MCVarInt (fromIntegral (BS.length encoded))) >> traverse_ poke (BS.unpack encoded) where
    encoded = encodeUtf8 text
  peek = do
    MCVarInt len <- peek :: Peek MCVarInt
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
    f Handshake { .. } = getSize (MCVarInt protocolVersion) + getSize (MCString serverAddress) + getSize serverPort + getSize (MCVarInt (fromIntegral (fromEnum nextState)))
  poke Handshake { .. } = poke (MCVarInt protocolVersion) >> poke (MCString serverAddress) >> poke serverPort >> poke (MCVarInt (fromIntegral (fromEnum nextState)))
  peek = do
    MCVarInt protocolVersion <- peek
    MCString serverAddress <- peek
    serverPort <- peek
    MCVarInt nextStateInt <- peek
    let nextState = toEnum (fromIntegral nextStateInt)
    return Handshake { .. }

data ServerState = HandshakingState | StatusState | LoginState | PlayState deriving (Show, Eq, Enum)

data ClientPacket (s :: ServerState) where
  ClientPacketHandshake :: Handshake -> ClientPacket HandshakingState
  ClientPacketRequest :: ClientPacket StatusState
  ClientPacketPing :: Int64 -> ClientPacket StatusState
  ClientPacketLoginStart :: Text -> ClientPacket LoginState

deriving instance Show (ClientPacket s)

instance Store (ClientPacket HandshakingState) where
  size = VarSize f where
    f :: ClientPacket HandshakingState -> Int
    f (ClientPacketHandshake handshake) = getSize (MCVarInt 0) + getSize handshake
  poke (ClientPacketHandshake handshake) = poke (MCVarInt 0) >> poke handshake
  peek = peek >>= \case
    MCVarInt 0 -> ClientPacketHandshake <$> peek

instance Store (ClientPacket StatusState) where
  size = VarSize f where
    f :: ClientPacket StatusState -> Int
    f ClientPacketRequest = getSize (MCVarInt 0)
    f (ClientPacketPing value) = getSize (MCVarInt 1) + getSize value
  poke ClientPacketRequest = poke (MCVarInt 0)
  poke (ClientPacketPing value) = poke (MCVarInt 1) >> poke value
  peek = peek >>= \case
    MCVarInt 0 -> pure ClientPacketRequest
    MCVarInt 1 -> ClientPacketPing <$> peek

instance Store (ClientPacket LoginState) where
  size = VarSize f where
    f :: ClientPacket LoginState -> Int
    f (ClientPacketLoginStart name) = getSize (MCVarInt 0) + getSize (MCString name)
  poke (ClientPacketLoginStart name) = poke (MCVarInt 0) >> poke (MCString name)
  peek = peek >>= \case
    MCVarInt 0 -> do
      MCString name <- peek
      pure $ ClientPacketLoginStart name

data ServerPacket (s :: ServerState) where
  ServerPacketResponse :: Response -> ServerPacket StatusState
  ServerPacketPong :: Int64 -> ServerPacket StatusState
  ServerPacketLoginSuccess :: Text -> Text -> ServerPacket LoginState
  ServerPacketDisconnect :: Text -> ServerPacket PlayState

deriving instance Show (ServerPacket s)

instance Store (ServerPacket StatusState) where
  size = VarSize f where
    f :: ServerPacket StatusState -> Int
    f (ServerPacketResponse response) = getSize (MCVarInt 0) + getSize (jsonToMCString response)
    f (ServerPacketPong value) = getSize (MCVarInt 1) + getSize value
  poke (ServerPacketResponse response) = poke (MCVarInt 0) >> poke (jsonToMCString response)
  poke (ServerPacketPong value) = poke (MCVarInt 1) >> poke value
  peek = peek >>= \case
    MCVarInt 0 -> do
      Just response <- mcStringToJson <$> peek
      return (ServerPacketResponse response)
    MCVarInt 1 -> ServerPacketPong <$> peek

instance Store (ServerPacket LoginState) where
  size = VarSize f where
    f :: ServerPacket LoginState -> Int
    f (ServerPacketLoginSuccess name uuid) = getSize (MCVarInt 2) + getSize (MCString uuid) + getSize (MCString name)
  poke (ServerPacketLoginSuccess name uuid) = poke (MCVarInt 2) >> poke (MCString uuid) >> poke (MCString name)
  peek = peek >>= \case
    MCVarInt 2 -> do
      MCString uuid <- peek
      MCString name <- peek
      pure $ ServerPacketLoginSuccess name uuid

instance Store (ServerPacket PlayState) where
  size = VarSize f where
    f :: ServerPacket PlayState -> Int
    f (ServerPacketDisconnect reason) = getSize (MCVarInt 0x1B) + getSize (jsonToMCString chat) where
      chat = Chat { chat_text = reason }

  poke (ServerPacketDisconnect reason) = poke (MCVarInt 0x1B) >> poke (jsonToMCString chat) where
    chat = Chat { chat_text = reason }
  peek = peek >>= \case
    MCVarInt 0x1B -> do
      Just Chat { chat_text = reason } <- mcStringToJson <$> peek
      pure $ ServerPacketDisconnect reason


jsonToMCString :: A.ToJSON a => a -> MCString
jsonToMCString = MCString . decodeUtf8 . LBS.toStrict . A.encode

mcStringToJson :: A.FromJSON a => MCString -> Maybe a
mcStringToJson (MCString text) = A.decodeStrict (encodeUtf8 text)

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

newtype Chat = Chat
  { chat_text :: Text
  } deriving (Generic, Show)

instance A.ToJSON Chat where
  toEncoding = A.genericToEncoding jsonOptions

instance A.FromJSON Chat where
  parseJSON = A.genericParseJSON jsonOptions

data Response = Response
  { response_version :: ResponseVersion
  , response_players :: ResponsePlayers
  , response_description :: Chat
  } deriving (Generic, Show)

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding jsonOptions

instance A.FromJSON Response where
  parseJSON = A.genericParseJSON jsonOptions
