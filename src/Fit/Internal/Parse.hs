{-|
Module      : Fit.Internal.Parse
Copyright   : Copyright 2014-2015, Matt Giles
License     : Modified BSD License (see LICENSE file)
Maintainer  : matt.w.giles@gmail.com
Stability   : experimental

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Fit.Internal.Parse (
  readFitRaw,
  parseFit,

  -- * Parsers for components of FIT files
  parseHeader,
  parseMessages,
  parseMessage,
  parseMessageDef,
  parseFieldDef,
  parseDataMessage,
  parseField,
  parseValue,
  parseArray,
  parseSeq,
  parseString,
  parseCTDataMessage,
  mkHeader
  ) where

import Fit.Internal.Architecture
import Fit.Internal.FitFile
import Fit.Internal.FitParser

import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Trans (lift)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A (parseOnly, string, anyWord8, takeTill)
import qualified Data.Attoparsec.Combinator as A (count, many')
import Data.Bits (testBit, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (init)
import Data.Sequence (Seq)
import qualified Data.Sequence as S (fromList)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word8)

import Prelude

-- TODO ignores final CRC bytes
-- | Parse a strict 'ByteString' containing the FIT data into a 'Fit' value
readFitRaw :: ByteString -> Either String Fit
readFitRaw bs = let noCrc = B.init (B.init bs) -- Drop CRC bytes
                in A.parseOnly parseFit noCrc

-- | An Attoparsec parser for 'Fit' values
parseFit :: Parser Fit
parseFit = runFitParser $ Fit <$> parseHeader <*> parseMessages

-- Numbers in the FIT header are all little-endian
parseHeader :: FitParser FitHeader
parseHeader = withArchitecture ArchLittle $ do
  headerSize <- word8
  protocolVersion <- word8
  profileVersion <- archWord16
  dataSize <- archWord32
  marker <- lift $ A.string ".FIT"
  crc <- if headerSize == 14
         then Just <$> archWord16
         else return Nothing

  return $ FH headerSize protocolVersion profileVersion dataSize marker crc

parseMessages :: FitParser [Message]
parseMessages = A.many' parseMessage

parseMessage :: FitParser Message
parseMessage = do
  headerByte <- lift A.anyWord8
  let header = mkHeader headerByte
  case header of
   DefHeader t -> do
     msgDef <- parseMessageDef t
     addMessageDef msgDef
     return (DefM msgDef)
   DataHeader t -> lookupMessageDef t >>= parseDataMessage
   CTDataHeader t o -> lookupMessageDef t >>= parseCTDataMessage o

parseMessageDef :: LocalMessageType -> FitParser MessageDefinition
parseMessageDef lmt = do
  _ <- word8 -- Unused reserved byte
  arch <- word8 >>= \case
           0 -> return ArchLittle
           1 -> return ArchBig
           _ -> error "Architecture neither 0 nor 1"
  globalNum <- fromIntegral <$> withArchitecture arch archInt16
  numFields <- fromIntegral <$> word8
  fieldDefs <- replicateM numFields (parseFieldDef)
  return $ MessageDef lmt globalNum arch fieldDefs

parseFieldDef :: FitParser FieldDef
parseFieldDef = FieldDef <$> num <*> size <*> baseType
  where num = fromIntegral <$> word8
        size = fromIntegral <$> word8
        baseType = word8 >>= return . \case
          0x00 -> FitEnum
          0x01 -> FitSInt8
          0x02 -> FitUInt8
          0x83 -> FitSInt16
          0x84 -> FitUInt16
          0x85 -> FitSInt32
          0x86 -> FitUInt32
          0x07 -> FitString
          0x88 -> FitFloat32
          0x89 -> FitFloat64
          0x0A -> FitUInt8Z
          0x8B -> FitUInt16Z
          0x8C -> FitUInt32Z
          0x0D -> FitByte
          _ -> error "Invalid base type field"

parseDataMessage :: MessageDefinition -> FitParser Message
parseDataMessage (MessageDef lmt gmt arch fieldDefs) = withArchitecture arch $ do
  fields <- mapM parseField fieldDefs
  return (DataM lmt gmt fields)

parseField :: FieldDef -> FitParser Field
parseField (FieldDef num size bt) = do
  let numValues = size `div` (btSize bt)

  field <- if numValues == 1 || (bt == FitString)
           then SingletonField num <$> parseValue bt
           else ArrayField num <$> parseArray numValues bt

  case field of
   TimestampField t -> storeTimestamp (Timestamp t) >> return field
   _ -> return field

parseValue :: BaseType -> FitParser Value
parseValue bt =
  case bt of
   FitEnum -> EnumValue <$> word8
   FitSInt8 -> SInt8Value <$> int8
   FitUInt8 -> UInt8Value <$> word8
   FitSInt16 -> SInt16Value <$> archInt16
   FitUInt16 -> UInt16Value <$> archWord16
   FitSInt32 -> SInt32Value <$> archInt32
   FitUInt32 -> UInt32Value <$> archWord32
   FitString -> StringValue <$> lift parseString
   FitFloat32 -> Float32Value <$> archFloat32
   FitFloat64 -> Float64Value <$> archFloat64
   FitUInt8Z -> UInt8ZValue <$> word8
   FitUInt16Z -> UInt16ZValue <$> archWord16
   FitUInt32Z -> UInt32ZValue <$> archWord32
   FitByte -> ByteValue <$> word8

-- | This function will fail if the 'BaseType' is 'FitString'. This implementation
-- currently doesn't support arrays of strings, but treats char arrays as always
-- being a single string.
parseArray :: Int         -- ^ The number of elements in the array
              -> BaseType -- ^ The base element type
              -> FitParser Array
parseArray n bt = let seqOf = parseSeq n
  in case bt of
   FitEnum -> EnumArray <$> seqOf word8
   FitSInt8 -> SInt8Array <$> seqOf int8
   FitUInt8 -> UInt8Array <$> seqOf word8
   FitSInt16 -> SInt16Array <$> seqOf archInt16
   FitUInt16 -> UInt16Array <$> seqOf archWord16
   FitSInt32 -> SInt32Array <$> seqOf archInt32
   FitUInt32 -> UInt32Array <$> seqOf archWord32
   FitString -> error "String arrays not supported -- how was this line reached?"
   FitFloat32 -> Float32Array <$> seqOf (fmap fromIntegral archInt32)
   FitFloat64 -> Float64Array <$> seqOf (fmap fromIntegral archInt64)
   FitUInt8Z -> UInt8ZArray <$> seqOf word8
   FitUInt16Z -> UInt16ZArray <$> seqOf archWord16
   FitUInt32Z -> UInt32ZArray <$> seqOf archWord32
   FitByte -> ByteArray <$> seqOf word8

-- | Run a 'FitParser' @n@ times to parse a 'Seq' of values
parseSeq :: Int -> FitParser a -> FitParser (Seq a)
parseSeq n p = S.fromList <$> A.count n p

-- | Parse a null-terminated UTF-8 string.
parseString :: Parser Text
parseString = decodeUtf8 <$> A.takeTill (== 0) <* A.anyWord8

-- | Parse a compressed-timestamp message, using the 'TimeOffset' from the
-- compressed-timestamp message header.
parseCTDataMessage :: TimeOffset -> MessageDefinition -> FitParser Message
parseCTDataMessage offset (MessageDef lmt gmt arch fieldDefs) = withArchitecture arch $ do
  fields <- mapM parseField fieldDefs
  newTimestamp <- updateTimestamp offset
  let timestampField = TimestampField (unTimestamp newTimestamp)
  return $ DataM lmt gmt (timestampField : fields)

-- | Transform a FIT message header byte into a 'MessageHeader'
mkHeader :: Word8 -> MessageHeader
mkHeader byte =
  if isNormalHeader
  then if isDefMessage
       then defHeader
       else dataHeader
  else ctDataHeader

  where
    isNormalHeader = not (testBit byte 7) -- 0 indicates normal header
    isDefMessage = testBit byte 6 -- 0 indicates data message
    defHeader = DefHeader normalLmt
    dataHeader = DataHeader normalLmt
    ctDataHeader = CTDataHeader ctLmt ctOffset
    normalLmt = mkLocalMessageType byte -- in a normal header the lmt is already low 4 bits
    ctLmt = mkLocalMessageType $ (byte `shiftR` 5) .&. 0x3 -- in CT header the lmt is bits 5 and 6
    ctOffset = mkTimeOffset byte -- in CT header the time offset is already low 5 bits

-- Originally this was exported from Fit.Internal.Types, but haddock chokes on patterns
-- between modules. I think the bug is fixed in GHC 7.8.4, so can move this back to
-- that module once we're off 7.8.3.
pattern TimestampField t = SingletonField 253 (UInt32Value t)
