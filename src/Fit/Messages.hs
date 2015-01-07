-- | The Messages API abtracts over the structure of a FIT file slightly and presents
-- the FIT file as just the sequence of data messages in the file. The Messages API
-- also abstracts over the various FIT base types (for example, signed/unsigned integers
-- of different sizes) to give a simpler set of types to work with.
--
-- If you need to know about the very specifics of the FIT file structure, use the Raw API
-- instead. However, for pulling information out of a FIT file this API is much more
-- convenient.
module Fit.Messages (
  readMessages,
  readFileMessages,
  parseMessages,
  Messages(..),
  Message(..),
  Field(..),
  Value(..),
  SingletonValue(..),
  ArrayValue(..)
  ) where

import qualified Fit.Internal.FitFile as Raw
import qualified Fit.Internal.Parse as Raw

import Control.Applicative ((<$>))
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile, pack)
import qualified Data.Foldable as F (toList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map (empty, insert)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import qualified Data.Sequence as S (fromList)
import Data.Text (Text)
import Data.Word (Word8)

-- | The collection of data messages from the FIT file.
newtype Messages = Messages {
  _messages :: Seq Message
  } deriving (Show)

-- | A FIT data message
data Message = Message {
  _mNumber :: !Int,        -- ^ The global message number, as found in the FIT profile
  _mFields :: IntMap Field -- ^ The fields in the message, mapped from field number to @Field@
  } deriving (Show)

-- | A single field in a FIT data message
data Field = Field {
  _fNumber :: !Int, -- ^ The field number, as found in the FIT profile
  _fValue  :: Value
  } deriving (Show)

-- | FIT values can either contain a single piece of data or an array. FIT arrays are homogenous
data Value = Singleton SingletonValue
           | Array ArrayValue
           deriving (Show)

-- | A singleton value. In the Messages API we abstract over the specific FIT base type of the field. For example, the FIT types uint8, sint8, uint16, etc. are all presented as an @IntValue@. FIT strings (ie. character arrays) are presented as singleton @TextValue@s. If you need to know the specific base type of a field you can use the Raw API.
data SingletonValue = IntValue !Int
                    | RealValue !Double
                    | ByteValue !Word8
                    | TextValue Text
                    deriving (Show)

-- | Array values. Like singleton values these ignore the specific FIT base type to present a simpler interface. Byte arrays are presented as strict @ByteString@s. There are no character arrays, since the singleton @TextValue@ handles that case.
data ArrayValue = IntArray (Seq Int)
                | RealArray (Seq Double)
                | ByteArray ByteString
                deriving (Show)

-- | Parse a strict @ByteString@ containing the FIT data into its @Messages@
readMessages :: ByteString -> Either String Messages
readMessages bs = toMessages <$> Raw.readFitRaw bs

-- | Parse the given FIT file into its @Messages@
readFileMessages :: FilePath -> IO (Either String Messages)
readFileMessages fp = B.readFile fp >>= return . readMessages

-- | An Attoparsec parser for @Messages@
parseMessages :: Parser Messages
parseMessages = fmap toMessages Raw.parseFit

toMessages :: Raw.Fit -> Messages
toMessages rFit = Messages . S.fromList . catMaybes $ fmap toMessage (Raw.fMessages rFit)

toMessage :: Raw.Message -> Maybe Message
toMessage (Raw.DefM _) = Nothing
toMessage (Raw.DataM _ gmt fields) = Just $ Message gmt (foldr go Map.empty fields)
  where go f@(Raw.SingletonField num _) fieldMap = Map.insert num (toField f) fieldMap
        go f@(Raw.ArrayField num _) fieldMap = Map.insert num (toField f) fieldMap

toField :: Raw.Field -> Field
toField (Raw.SingletonField num value) = Field num . Singleton $ (fromSingletonValue value)
toField (Raw.ArrayField num array) = Field num . Array $ (fromArray array)

fromSingletonValue :: Raw.Value -> SingletonValue
fromSingletonValue v =
  case v of
   Raw.EnumValue i -> IntValue (fromIntegral i)
   Raw.SInt8Value i -> IntValue (fromIntegral i)
   Raw.UInt8Value i -> IntValue (fromIntegral i)
   Raw.SInt16Value i -> IntValue (fromIntegral i)
   Raw.UInt16Value i -> IntValue (fromIntegral i)
   Raw.SInt32Value i -> IntValue (fromIntegral i)
   Raw.UInt32Value i -> IntValue (fromIntegral i)
   Raw.StringValue t -> TextValue t
   Raw.Float32Value f -> RealValue (fromRational (toRational f))
   Raw.Float64Value f -> RealValue f
   Raw.UInt8ZValue i -> IntValue (fromIntegral i)
   Raw.UInt16ZValue i -> IntValue (fromIntegral i)
   Raw.UInt32ZValue i -> IntValue (fromIntegral i)
   Raw.ByteValue b -> ByteValue b

fromArray :: Raw.Array -> ArrayValue
fromArray a =
  case a of
   Raw.EnumArray xs -> intArray xs
   Raw.SInt8Array xs -> intArray xs
   Raw.UInt8Array xs -> intArray xs
   Raw.SInt16Array xs -> intArray xs
   Raw.UInt16Array xs -> intArray xs
   Raw.SInt32Array xs -> intArray xs
   Raw.UInt32Array xs -> intArray xs
   Raw.Float32Array xs -> realArray xs
   Raw.Float64Array xs -> realArray xs
   Raw.UInt8ZArray xs -> intArray xs
   Raw.UInt16ZArray xs -> intArray xs
   Raw.UInt32ZArray xs -> intArray xs
   Raw.ByteArray bs -> ByteArray . B.pack $ F.toList bs
  where intArray is = IntArray $ fmap fromIntegral is
        realArray rs = RealArray $ fmap (fromRational . toRational) rs
