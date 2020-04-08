{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Minecraft
import Test.Hspec
import Data.Store
import qualified Data.ByteString as BS
import Data.ByteString (unpack)
import Data.Word
import Data.Int
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

varIntSpec :: Spec
varIntSpec = describe "Minecraft.MCVarInt" $ do
  it "correctly handles the wiki examples" $
    forM_ wikiExamples $ \(value, bytes) ->
      unpack (encode (MCVarInt value)) `shouldBe` bytes

  it "doesn't change the value when encoding and decoding" $
    property $ \value -> decode (encode (MCVarInt value)) == Right (MCVarInt value)

  it "has a maximum size of 5" $
    property $ \value -> BS.length (encode (MCVarInt value)) <= 5

  where
    -- These are from https://wiki.vg/Protocol#VarInt_and_VarLong
    wikiExamples :: [(Int32, [Word8])] =
      [ (0, [0])
      , (1, [1])
      , (2, [2])
      , (127, [127])
      , (128, [128, 1])
      , (255, [255, 1])
      , (2147483647, [255, 255, 255, 255, 7])
      , (-1, [255, 255, 255, 255, 15])
      , (-2147483648, [128, 128, 128, 128, 8])
      ]

mcStringSpec :: Spec
mcStringSpec = describe "Minecraft.MCString" $
  it "doesn't change the value when encoding and decoding" $
    property $ \text -> decode (encode (MCString text)) == Right (MCString text)

main :: IO ()
main = hspec $ do
  varIntSpec
  mcStringSpec
