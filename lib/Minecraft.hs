{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BinaryLiterals #-}

module Minecraft (VarInt(..), MCString(..))where

import Data.Store
import Data.Int (Int32)
import Data.Bits
import Data.Word (Word8, Word32)
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS

import Data.Text
import Data.Text.Encoding
import Data.Foldable (traverse_)
import Control.Monad (replicateM)

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
