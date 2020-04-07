{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Minecraft where

import Data.Store
import Data.Store.Internal (combineSize, combineSizeWith, getSizeWith)
import Data.Int (Int32, Int64)
import Data.Bits
import Data.Word (Word8, Word32, Word16)
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

type ProtocolVersion = VarInt
type ServerAddress = MCString
type ServerPort = Word16
type ConnectionState = VarInt

data Handshake = Handshake
  { protocolVersion :: ProtocolVersion
  , serverAddress :: ServerAddress
  , serverPort :: ServerPort
  , nextState :: ConnectionState
  } deriving (Generic, Show, Eq)

instance Store Handshake
testHandshake = Handshake (VarInt 10) (MCString "hello") 10 (VarInt 10)

newtype HandshakePacket = HandshakePacket { handshakePacketData :: Handshake }
  deriving (Show, Eq)

type PacketID = VarInt
handshakePacketId :: HandshakePacket -> PacketID
handshakePacketId (HandshakePacket _) = VarInt 0

instance Store HandshakePacket where
  size = combineSize handshakePacketId handshakePacketData
  poke value = poke (handshakePacketId value) >> poke (handshakePacketData value)
  peek = do
    VarInt readPacketId :: PacketID <- peek
    case readPacketId of
      0 -> HandshakePacket <$> peek
      _ -> fail $ "Packet ID " <> show readPacketId <> " not supported"

data ClientStatusPacket = ClientStatusRequest | ClientStatusPing Int64

instance Store ClientStatusPacket where
  size = choose f (combineSize (const (VarInt 0)) id) (combineSize (const (VarInt 1)) id) where
    f :: ClientStatusPacket -> Either () Int64
    f ClientStatusRequest = Left ()
    f (ClientStatusPing x) = Right x
  poke ClientStatusRequest = poke (VarInt 0)
  poke (ClientStatusPing val) = poke (VarInt 1) >> poke val
  peek = peek >>= \case
    VarInt 0 -> pure ClientStatusRequest
    VarInt 1 -> ClientStatusPing <$> peek


data ServerStatusPacket = ServerStatusResponse MCString | ServerStatusPong Int64 deriving Show

instance Store ServerStatusPacket where
  size = choose f (combineSize (const (VarInt 0)) id) (combineSize (const (VarInt 1)) id) where
    f :: ServerStatusPacket -> Either MCString Int64
    f (ServerStatusResponse val) = Left val
    f (ServerStatusPong x) = Right x
  poke (ServerStatusResponse val) = poke (VarInt 0) >> poke val
  poke (ServerStatusPong val) = poke (VarInt 1) >> poke val
  peek = peek >>= \case
    VarInt 0 -> ServerStatusResponse <$> peek
    VarInt 1 -> ServerStatusPong <$> peek

instance Divisible Size where
  divide f = combineSizeWith (fst . f) (snd . f)
  conquer = ConstSize 0

instance Decidable Size where
  choose f s t = VarSize (either (getSizeWith s) (getSizeWith t) . f)
  lose f = VarSize (absurd . f)

instance Num (Size a) where
  VarSize l + VarSize r = VarSize (liftA2 (+) l r)
  VarSize l + ConstSize r = VarSize ((+ r) <$> l)
  ConstSize l + VarSize r = VarSize ((+ l) <$> r)
  ConstSize l + ConstSize r = ConstSize (l + r)

  VarSize l * VarSize r = VarSize (liftA2 (*) l r)
  VarSize l * ConstSize r = VarSize ((* r) <$> l)
  ConstSize l * VarSize r = VarSize ((* l) <$> r)
  ConstSize l * ConstSize r = ConstSize (l * r)

  abs (ConstSize v) = ConstSize (abs v)
  abs (VarSize f) = VarSize (abs <$> f)

  signum (ConstSize v) = ConstSize (signum v)
  signum (VarSize f) = VarSize (signum <$> f)

  fromInteger int = ConstSize (fromInteger int)

  negate (ConstSize v) = ConstSize (negate v)
  negate (VarSize f) = VarSize (negate <$> f)

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

data ResponsePlayers = ResponsePlayers
  { response_players_max :: Int
  , response_players_online :: Int
  , response_players_sample :: [()]
  } deriving (Generic, Show)

instance A.ToJSON ResponsePlayers where
  toEncoding = A.genericToEncoding jsonOptions

newtype ResponseDescription = ResponseDescription
  { response_description_text :: Text
  } deriving (Generic, Show)

instance A.ToJSON ResponseDescription where
  toEncoding = A.genericToEncoding jsonOptions

data Response = Response
  { response_version :: ResponseVersion
  , response_players :: ResponsePlayers
  , response_description :: ResponseDescription
  } deriving (Generic, Show)

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding jsonOptions
