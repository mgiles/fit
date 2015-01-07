module Test.Fit.Internal.Numbers (
  specs
  ) where

import Fit.Internal.Numbers

import Test.Hspec
import Test.Hspec.Attoparsec
import Test.QuickCheck

import Data.Bits
import Data.Int
import Data.Word
import qualified Data.ByteString as B

specs :: Spec
specs = describe "Fit.Parse.Numbers" $ do
  nByteIntLeSpec
  nByteIntBeSpec

nByteIntLeSpec :: Spec
nByteIntLeSpec = describe "Little endian" $ do
  it "Correctly parses a single byte" $ do
    property $ \n -> B.pack [n] ~> (nByteIntLe 1)
      `shouldParse` (fromIntegral n :: Word8)

  it "Correctly parses a multi-byte Int" $ do
    property $ \n m -> B.pack[n,m] ~> (nByteIntLe 2)
      `shouldParse` ((fromIntegral m `shiftL` 8) + (fromIntegral n) :: Int16)

nByteIntBeSpec :: Spec
nByteIntBeSpec = describe "Big endian" $ do
  it "Correctly parses a single byte" $ do
    property $ \n -> B.pack [n] ~> (nByteIntBe 1)
      `shouldParse` (fromIntegral n :: Word8)

  it "Correctly parses a multi-byte Int" $ do
    property $ \n m -> B.pack[n,m] ~> (nByteIntBe 2)
      `shouldParse` ((fromIntegral n `shiftL` 8) + (fromIntegral m) :: Int16)
