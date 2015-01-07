module Test.Fit.Internal.FitParser (
  specs
  ) where

import Fit.Internal.Architecture
import Fit.Internal.FitFile
import Fit.Internal.FitParser

import Test.Hspec
import Test.Hspec.Attoparsec
import Test.QuickCheck hiding ((.&.))

import Data.Bits
import qualified Data.ByteString as B
import Data.Word

specs :: Spec
specs = describe "Fit.Parse.FitParser" $ do
  architectureSpec
  messageDefsSpec
  timestampSpec

architectureSpec :: Spec
architectureSpec = describe "Architecture" $ do
  it "Parses a single byte" $ property $
    \n -> B.pack [n] ~> runFitParser word8 `shouldParse` n

  it "Parses little-endian by default" $ property $
    \n -> B.pack [n,0] ~> runFitParser archWord16 `shouldParse` (fromIntegral n :: Word16)

  it "Respects withArchitecture when parsing" $ property $
    let parser = runFitParser $ withArchitecture ArchBig archWord16
    in \n -> B.pack [n,0] ~> parser `shouldParse` (fromIntegral n `shiftL` 8 :: Word16)

  it "Nests withArchitecture calls correctly" $ do
    let parser = withArchitecture ArchBig $ do
          little <- withArchitecture ArchLittle archWord16
          big <- archWord16
          return (little, big)
    B.pack [1,0,1,0] ~> runFitParser parser `shouldParse` (1, 256)

messageDefsSpec :: Spec
messageDefsSpec = describe "Message definitions" $ do
  it "Finds the correct message def" $ do
    let targetDef = MessageDef (LMT 0) 0 ArchLittle []
        extraDef = MessageDef (LMT 1) 1 ArchLittle []
        parser = runFitParser $ do
          addMessageDef targetDef
          addMessageDef extraDef
          lookupMessageDef (LMT 0)

    B.empty ~> parser `shouldParse` targetDef

  it "Replaces message def with same local message type" $ do
    let oldDef = MessageDef (LMT 0) 0 ArchLittle []
        newDef = MessageDef (LMT 0) 1 ArchBig []
        parser = runFitParser $ do
          addMessageDef oldDef
          addMessageDef newDef
          lookupMessageDef (LMT 0)

    B.empty ~> parser `shouldParse` newDef

timestampSpec :: Spec
timestampSpec = describe "Timestamps" $ do
  -- Compressed timestamp offsets aren't zero-based. See FitParser.updateTimestamp
  -- or the FIT protocol document for details.
  it "Finds the previously stored Timestamp" $ do
    let timestamp = Timestamp 100
        nullOffset = TO . fromIntegral $ unTimestamp timestamp .&. 0x1F
        parser = runFitParser $ storeTimestamp timestamp >> updateTimestamp nullOffset

    B.empty ~> parser `shouldParse` timestamp

  -- Data from Figure 4.2 in the FIT spec rev. 1.7
  it "Decompresses timestamp series correctly" $ do
    let base = Timestamp 0x3B
        offsets = map TO [0x1B, 0x1D, 0x02, 0x05, 0x01]
        expectedStamps = map Timestamp [0x3B, 0x3D, 0x42, 0x45, 0x61]
        parser = runFitParser $ do
          storeTimestamp base
          mapM updateTimestamp offsets

    B.empty ~> parser `shouldParse` expectedStamps
