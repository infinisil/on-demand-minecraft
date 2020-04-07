{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BinaryLiterals #-}

module Minecraft (VarInt(..))where

import Data.Store
import Data.Int (Int32)
import Data.Bits
import Data.Word (Word8, Word32)
import Data.Maybe (fromJust)

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
