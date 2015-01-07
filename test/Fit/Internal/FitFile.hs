module Test.Fit.Internal.FitFile (
  specs
  ) where

import Fit.Internal.FitFile

import Test.Hspec
import Test.QuickCheck hiding ((.&.))

import Data.Bits

specs :: Spec
specs = describe "Fit.Types.Message" $ do
  localMessageTypeSpec
  timeOffsetSpec

localMessageTypeSpec :: Spec
localMessageTypeSpec = describe "Local message type" $ do
  it "Is smaller than 16" $ do
    property $ \n -> (unLmt . mkLocalMessageType) (n :: Int) < 16

  it "Uses the low 4 bits in the constructor" $ do
    property $ \n -> (unLmt . mkLocalMessageType) (n :: Int) == fromIntegral (n .&. 0x0F)

timeOffsetSpec :: Spec
timeOffsetSpec = describe "Time offset" $ do
  it "Is smaller than 32" $ do
    property $ \n -> (unTo . mkTimeOffset) (n :: Int) < 32

  it "Uses the low 5 bits in the constructor" $ do
    property $ \n -> (unTo . mkTimeOffset) (n :: Int) == fromIntegral (n .&. 0x1F)
