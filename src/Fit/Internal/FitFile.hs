module Fit.Internal.FitFile (
  Fit(..),
  FitHeader(..),
  Message(..),
  msgLmt,
  MessageDefinition(..),
  FieldDef(..),
  Field(..),
  Value(..),
  Array(..),
  MessageHeader(..),
  LocalMessageType(..),
  mkLocalMessageType,
  unLocalMessageType,
  TimeOffset(..),
  mkTimeOffset,
  Timestamp(..),
  BaseType(..),
  btSize
  ) where

import Fit.Internal.Architecture

import Data.Bits (Bits, (.&.))
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32)

-- | A FIT file consists of a header and a collection of messages.
data Fit = Fit {
  fHeader :: FitHeader,
  fMessages :: [Message]
  } deriving (Show)

-- | The FIT file header
data FitHeader = FH {
  fhSize            :: !Word8,         -- ^ Size of the header in bytes. Will always be 12 or 14,
                                       -- based on presence of the CRC

  fhProtocolVersion :: !Word8,         -- ^ Protocol version number

  fhProfileVersion  :: !Word16,        -- ^ Profile version number

  fhDataSize        :: !Word32,        -- ^ Combined length of the FIT messages, in bytes

  fhDataType        :: ByteString,     -- ^ File tag, should always be ".FIT"

  fhCrc             :: !(Maybe Word16) -- ^ Optional checksum for header contents
  } deriving (Show)

-- | There are two kinds of FIT messages:
--
--     * __Definition messages__ set the structure for messages of a particular /local message type/
--     * __Data messages__ contain the actual information, according to the structure given by a
--       definition message.
data Message = DefM MessageDefinition
             | DataM !LocalMessageType !Int [Field]
             deriving (Show)

msgLmt :: Message -> LocalMessageType
msgLmt (DefM (MessageDef lmt _ _ _)) = lmt
msgLmt (DataM lmt _ _) = lmt

-- | A @MessageDefinition@ for a local message type (LMT) determines how future data messages with
-- that LMT are decoded. LMTs can be re-used: a data message with LMT @n@ will use the /most recent/
-- message definition for LMT @n@.
data MessageDefinition = MessageDef {
  defLocalType  :: !LocalMessageType, -- ^ The local message type being defined

  defGlobalType :: !Int,              -- ^ The /global/ message type this LMT will refer to. Must be a
                                      -- valid @mesg_num@ value from the FIT profile

  defArch       :: !Arch,             -- ^ The /architecture/ this messages with this LMT will use
                                      -- for multi-byte values (little- or big-endian)

  defFields     :: [FieldDef]         -- ^ Definitions for the fields messages with this LMT will contain
  } deriving (Show, Eq)

-- | Defines the structure for a single field in a message
data FieldDef = FieldDef {
  fdNum      :: !Int,     -- ^ The /field number/. The interpretation of the field number depends on the
                          -- global message type and is found in the FIT profile

  fdSize     :: !Int,     -- ^ The size, in bytes, of the field's contents. This will be a multiple of
                          -- the base type size. In a singleton field this size will be the same as the
                          -- base type size. In an array field it will be some multiple of the base type
                          -- size.

  fdBaseType :: !BaseType -- ^ The FIT base type of values in the field
  } deriving (Show, Eq)

-- | A single field in a data message, containing the field number and the value(s)
data Field = SingletonField !Int Value
           | ArrayField !Int Array
           deriving (Show)

-- | Singleton values. There is a Value constructor for each BaseType constructor. The wrapped
-- value in these constructors corresponds to the specific format used in the FIT file, for
-- example an 'enum' in FIT is stored as an 8-bit unsigned int (ie a Word8). The primary exception
-- to this is using @Text@ for string values.
data Value = EnumValue !Word8
           | SInt8Value !Int8
           | UInt8Value !Word8
           | SInt16Value !Int16
           | UInt16Value !Word16
           | SInt32Value !Int32
           | UInt32Value !Word32
           | StringValue Text -- ^ A 'string' in FIT is a null-terminated arrays of UTF-8
                              -- code units, but @Text@ is used here instead
           | Float32Value !Float
           | Float64Value !Double
           | UInt8ZValue !Word8
           | UInt16ZValue !Word16
           | UInt32ZValue !Word32
           | ByteValue !Word8
           deriving (Show)

-- | Array values use similar constructors to singleton values. However, there is no constructor
-- for arrays of strings, which seem to be unused in FIT.
data Array = EnumArray (Seq Word8)
           | SInt8Array (Seq Int8)
           | UInt8Array (Seq Word8)
           | SInt16Array (Seq Int16)
           | UInt16Array (Seq Word16)
           | SInt32Array (Seq Int32)
           | UInt32Array (Seq Word32)
           | Float32Array (Seq Float)
           | Float64Array (Seq Double)
           | UInt8ZArray (Seq Word8)
           | UInt16ZArray (Seq Word16)
           | UInt32ZArray (Seq Word32)
           | ByteArray (Seq Word8)
           deriving (Show)

-- | Each message has a header that primarily determines whether the message is a
-- definition or data message. If the message uses a /compressed timestamp header/,
-- the header also contains the compressed time offset.
data MessageHeader = DefHeader !LocalMessageType
                   | DataHeader !LocalMessageType
                   | CTDataHeader !LocalMessageType !TimeOffset
                   deriving (Show)

-- | A local message type is a 4 bit unsigned integer
newtype LocalMessageType = LMT { unLmt :: Int8 } deriving (Show, Eq)

-- | Only the lower 4 bits of the integer are used to construct a @LocalMessageType@
mkLocalMessageType :: (Integral a, Bits a) => a -> LocalMessageType
mkLocalMessageType = LMT . fromIntegral . ((.&.) 0xF)

-- | Unwrap a @LocalMessageType@. The resulting integer will be between 0 and 15
unLocalMessageType :: Integral a => LocalMessageType -> a
unLocalMessageType = fromIntegral . unLmt

-- | A time offset is 5 bits
newtype TimeOffset = TO { unTo :: Word8 } deriving (Show)

-- | Only the lower 5 bits of the number are used
mkTimeOffset :: (Integral a, Bits a) => a -> TimeOffset
mkTimeOffset = TO . fromIntegral . ((.&.) 0x1F)

newtype Timestamp = Timestamp { unTimestamp :: Word32 } deriving (Show, Eq)

-- | The different types that FIT uses for field values
data BaseType = FitEnum
              | FitSInt8
              | FitUInt8
              | FitSInt16
              | FitUInt16
              | FitSInt32
              | FitUInt32
              | FitString
              | FitFloat32
              | FitFloat64
              | FitUInt8Z
              | FitUInt16Z
              | FitUInt32Z
              | FitByte
              deriving (Show, Eq)

-- | Get the size in bytes for a single value of the given base type
btSize :: BaseType -> Int
btSize bt = case bt of
                FitEnum -> 1
                FitSInt8 -> 1
                FitUInt8 -> 1
                FitSInt16 -> 2
                FitUInt16 -> 2
                FitSInt32 -> 4
                FitUInt32 -> 4
                FitString -> 1
                FitFloat32 -> 4
                FitFloat64 -> 8
                FitUInt8Z -> 1
                FitUInt16Z -> 2
                FitUInt32Z -> 4
                FitByte -> 1
