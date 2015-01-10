{-|
Module      : Fit.Messages.Lens
Copyright   : Copyright 2014-2015, Matt Giles
License     : Modified BSD License (see LICENSE file)
Maintainer  : matt.w.giles@gmail.com
Stability   : experimental

Some basic lenses for the Messages API. These are compatible with both lens and lens-family.
This package doesn't provide any lens combinators like @^.@ or @^..@, so you'll need to use
ones from a lens package.

For example, the following code gets the values of the 'speed' fields
from all of the 'record' messages in the file:

@
Right fit <- readFileMessages "file.fit"
let speeds = fit ^.. message 20 . field 6 . int
@
-}

module Fit.Messages.Lens (
  -- * Messages
  messages,
  message,
  messageNumber,

  -- * Fields
  fields,
  field,
  fieldNumber,
  fieldValue,

  -- * Values
  -- $values
  int,
  real,
  text,
  byte,
  ints,
  reals,
  bytestring
  ) where

import Fit.Messages

import Control.Applicative ((<$>), pure, Applicative)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant (Contravariant, contramap)
import qualified Data.IntMap as Map (filterWithKey)
import Data.Sequence (Seq)
import qualified Data.Sequence as S (filter)
import Data.Text (Text)
import Data.Traversable (traverse)
import Data.Word (Word8)

-- Helper for building Folds
coerce :: (Contravariant f, Applicative f) => f a -> f b
coerce = contramap (const ()) . fmap (const ())

-- | Traverse all the messages in a 'Messages'
--
-- @messages :: Traversal' Messages Message@
messages :: Applicative f => (Message -> f Message) -> Messages -> f Messages
messages f ms =  Messages <$> traverse f (_messages ms)
{-# INLINE messages #-}

-- | A Fold over the messages with the given message number
--
-- @message :: Int -> Fold Messages Message@
message :: (Contravariant f, Applicative f) => Int -> (Message -> f Message) -> Messages -> f Messages
message msgNum f ms = coerce (traverse f targets)
  where targets = S.filter ((== msgNum) . _mNumber) (_messages ms)
{-# INLINE message #-}

-- | Lens on the message number from a 'Message'
--
-- @messageNumber :: Lens' Message Int@
messageNumber :: Functor f => (Int -> f Int) -> Message -> f Message
messageNumber f m = (\n -> m { _mNumber = n }) <$> f (_mNumber m)
{-# INLINE messageNumber #-}

-- | Traverse all the fields in a 'Message'
--
-- @fields :: Traversal' Message Field@
fields :: Applicative f => (Field -> f Field) -> Message -> f Message
fields f (Message n flds) = Message n <$> traverse f flds
{-# INLINE fields #-}

-- | A Fold over the fields in a 'Message' with the given field number
--
-- @field :: Int -> Fold Message Field@
field :: (Contravariant f, Applicative f) => Int -> (Field -> f Field) -> Message -> f Message
field n f msg = coerce (traverse f targetFields)
  where targetFields = Map.filterWithKey (\k _ -> k == n) (_mFields msg)
{-# INLINE field #-}

-- | Lens on the field number from a 'Field'
--
-- @fieldNumber :: Lens Field Int@
fieldNumber :: Functor f => (Int -> f Int) -> Field -> f Field
fieldNumber f fld = (\n -> fld { _fNumber = n }) <$> f (_fNumber fld)
{-# INLINE fieldNumber #-}

-- | Lens on the 'Value' from a 'Field'
--
-- @fieldValue :: Lens Field Value@
fieldValue :: Functor f => (Value -> f Value) -> Field -> f Field
fieldValue f fld = (\v -> fld { _fValue = v }) <$> f (_fValue fld)
{-# INLINE fieldValue #-}


-- $values
-- Generally when you're looking up the value for a particular field you'll know
-- the expected type ahead of time. If you know the field you're looking at holds
-- integers, then you can use 'int' to directly get an 'Int' instead of a
-- @Singleton (IntValue x)@.
--
-- These traversals are not prisms, because to reconstruct the 'Field' we need
-- the field number in addition to the wrapped value.

-- | Traverse the 'Singleton' and 'IntValue' constructors for a field value
--
-- @int :: Traversal' Field Int@
int :: Applicative f => (Int -> f Int) -> Field -> f Field
int f (Field n (Singleton (IntValue i))) = Field n . Singleton . IntValue <$> f i
int _ fld = pure fld
{-# INLINE int #-}

-- | Traverse the 'Singleton' and 'RealValue' constructors for a field value
--
-- @real :: Traversal' Field Double@
real :: Applicative f => (Double -> f Double) -> Field -> f Field
real f (Field n (Singleton (RealValue d))) = Field n . Singleton . RealValue <$> f d
real _ fld = pure fld
{-# INLINE real #-}

-- | Traverse the 'Singleton' and 'TextValue' constructors for a field value
--
-- @text :: Traversal' Field Text@
text :: Applicative f => (Text -> f Text) -> Field -> f Field
text f (Field n (Singleton (TextValue t))) = Field n . Singleton . TextValue <$> f t
text _ fld = pure fld
{-# INLINE text #-}

-- | Traverse the 'Singleton' and 'ByteValue' constructors for a field value
--
-- @byte :: Traversal' Field Word8@
byte :: Applicative f => (Word8 -> f Word8) -> Field -> f Field
byte f (Field n (Singleton (ByteValue b))) = Field n . Singleton . ByteValue <$> f b
byte _ fld = pure fld
{-# INLINE byte #-}

-- | Traverse the 'Array' and 'IntArray' constructors for a field value
--
-- @ints :: Traversal' Field (Seq Int)@
ints :: Applicative f => (Seq Int -> f (Seq Int)) -> Field -> f Field
ints f (Field n (Array (IntArray s))) = Field n . Array . IntArray <$> f s
ints _ fld = pure fld
{-# INLINE ints #-}

-- | Traverse the 'Array' and 'RealArray' constructors for a field value
--
-- @reals :: Traversal' Field (Seq Double)@
reals :: Applicative f => (Seq Double -> f (Seq Double)) -> Field -> f Field
reals f (Field n (Array (RealArray s))) = Field n . Array . RealArray <$> f s
reals _ fld = pure fld
{-# INLINE reals #-}

-- | Travese the 'Array' and 'ByteArray' constructors for a field value
--
-- @bytestring :: Traversal' Field ByteString@
bytestring :: Applicative f => (ByteString -> f ByteString) -> Field -> f Field
bytestring f (Field n (Array (ByteArray bs))) = Field n . Array . ByteArray <$> f bs
bytestring _ fld = pure fld
{-# INLINE bytestring #-}
