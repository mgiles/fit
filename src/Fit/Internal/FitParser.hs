{-|
Module      : Fit.Internal.FitParser
Copyright   : Copyright 2014-2015, Matt Giles
License     : Modified BSD License (see LICENSE file)
Maintainer  : matt.w.giles@gmail.com
Stability   : experimental

-}

{-# LANGUAGE LambdaCase #-}

module Fit.Internal.FitParser (
  -- * FitParser
  FitParser,
  runFitParser,
  FpState(..),
  Definitions(..),

  addMessageDef,
  lookupMessageDef,
  withArchitecture,
  storeTimestamp,
  updateTimestamp,

  -- * Architecture-independent parsers
  word8,
  int8,

  -- * Architecture-dependent parsers
  -- $archparsers
  archWord16,
  archWord32,
  archWord64,

  archInt16,
  archInt32,
  archInt64,

  archFloat32,
  archFloat64
  ) where

import Fit.Internal.Architecture
import Fit.Internal.FitFile
import qualified Fit.Internal.Numbers as N

import Control.Applicative
import Control.Monad.State.Class (get, modify)
import Control.Monad.State.Strict (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A (anyWord8)
import Data.Bits ((.&.))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap (insert, lookup, empty)
import Data.Word (Word8, Word16, Word32, Word64)

import Prelude

type FitParser a = StateT FpState Parser a

-- | Turn a 'FitParser' into a plain attoparsec 'Parser'. This doesn't require any
-- configuration as the initial state for a FIT parse is always the same.
runFitParser :: FitParser a -> Parser a
runFitParser = flip evalStateT (FpState ArchLittle defEmpty Nothing)

-- | Little-endian interpretation is used by default by 'FitParser'.
-- Use this function to set the endianness to use for the scope of a particular action.
-- After the action is finished the previous endianness is restored.
withArchitecture :: Arch -> FitParser a -> FitParser a
withArchitecture arch action =
  use fpArch >>= \old -> setArch arch *> action <* setArch old

setArch :: Arch -> FitParser ()
setArch = assign fpArch

-- | Register a 'MessageDefinition' with the parser, so it can decode
-- subsequent data messages using the definition
addMessageDef :: MessageDefinition -> FitParser ()
addMessageDef def = fpMessageDefs %= (defAdd def)

-- | Look up the 'MessageDefinition' for the given message type.
-- It is an error to look up a message type that has no registered definition,
-- since it is impossible to decode a data message with no definition
lookupMessageDef :: LocalMessageType -> FitParser MessageDefinition
lookupMessageDef lmt = do
  msgDefs <- use fpMessageDefs
  case defLookup lmt msgDefs of
   Just def -> return def
   Nothing -> error $ "No definition for local type " ++ (show lmt)

-- | Store the given 'Timestamp' as the most recent. Is used to store timestamps
-- from non-compressed timestamp messages. For compressed-timestamp messages use
-- 'updateTimestamp' instead.
storeTimestamp :: Timestamp -> FitParser ()
storeTimestamp t = fpLastTimestamp .= Just t

-- | Use the given 'TimeOffset' and the previous 'Timestamp' to compute a new
-- Timestamp. The new 'Timestamp' is stored as most recent and is returned.
--
-- This function fails if there is no previously-stored 'Timestamp'. This
-- condition should never come up when parsing a valid FIT file.
updateTimestamp :: TimeOffset -> FitParser Timestamp
updateTimestamp offset = do
  previous <- use fpLastTimestamp
  let new = addOffset offset previous
  fpLastTimestamp .= Just new
  return new

  where addOffset _ Nothing = error "No base timestamp to update"
        addOffset (TO off) (Just (Timestamp previous)) =
          let off' = fromIntegral off
              low5Prev = previous .&. 0x1F
              high27Prev = previous .&. 0xFFFFFFE0
              rollover = off' < low5Prev
          in if rollover
             then Timestamp $ high27Prev + 0x20 + off'
             else Timestamp $ high27Prev + off'

-- | The necessary state for parsing FIT files
data FpState = FpState {
  _fpArch          :: !Arch,             -- ^ The active endian-ness
  _fpMessageDefs   :: Definitions,       -- ^ The set of active message definitions
  _fpLastTimestamp :: !(Maybe Timestamp) -- ^ The most recently stored timestamp
  }

-- | The definitions are stored as a map on the local message type number. When a definition
-- is parsed with a previously-used local message type, the previous definition is
-- overwritten.
newtype Definitions = Defs { unDefs :: IntMap MessageDefinition }

{- Lenses for FpState -}
fpArch :: Functor f => Lens f FpState Arch
fpArch f (FpState arch defs ts) = (\arch' -> FpState arch' defs ts) <$> (f arch)

fpMessageDefs :: Functor f => Lens f FpState Definitions
fpMessageDefs f (FpState arch defs ts) = (\defs' -> FpState arch defs' ts) <$> (f defs)

fpLastTimestamp :: Functor f => Lens f FpState (Maybe Timestamp)
fpLastTimestamp f (FpState arch defs ts) = (\ts' -> FpState arch defs ts') <$> (f ts)

defAdd :: MessageDefinition -> Definitions -> Definitions
defAdd md = Defs . (IntMap.insert lmt md) . unDefs
  where lmt = unLocalMessageType . defLocalType $ md

defLookup :: LocalMessageType -> Definitions -> Maybe MessageDefinition
defLookup (LMT lmt) (Defs defs) = IntMap.lookup (fromIntegral lmt) defs

defEmpty :: Definitions
defEmpty = Defs (IntMap.empty)


word8 :: FitParser Word8
word8 = lift A.anyWord8

int8 :: FitParser Int8
int8 = fromIntegral <$> word8

-- $archparsers
-- The following parsers are all sensitive to the active endianness. For example,
-- 'archWord16' will use a little-endian or big-endian interpretation according
-- to the architecture for the 'MessageDefinition' for the current message.
-- Internally, these parsers use the endian-specific parsers from "Fit.Internal.Numbers".

-- | Parse a Word16 using the active endianness
archWord16 :: FitParser Word16
archWord16 = withArch N.word16le N.word16be

-- | Parse a Word32 using the active endianness
archWord32 :: FitParser Word32
archWord32 = withArch N.word32le N.word32be

-- | Parse a Word64 using the active endianness
archWord64 :: FitParser Word64
archWord64 = withArch N.word64le N.word64be

-- | Parse an Int16 using the active endianness
archInt16 :: FitParser Int16
archInt16 = withArch N.int16le N.int16be

-- | Parse an Int32 using the active endiannessa
archInt32 :: FitParser Int32
archInt32 = withArch N.int32le N.int32be

-- | Parse an Int64 using the active endianness
archInt64 :: FitParser Int64
archInt64 = withArch N.int64le N.int64be

-- | Parse a Float using the active endianness
archFloat32 :: FitParser Float
archFloat32 = withArch N.float32le N.float32be

-- | Parse a Double using the active endianness
archFloat64 :: FitParser Double
archFloat64 = withArch N.float64le N.float64be

-- | Perform an architecture-sensitive operation with separate
-- actions for little- and big-endian parsing
withArch :: LittleEndian (Parser a) -> BigEndian (Parser a) -> FitParser a
withArch little big = use fpArch >>= \case
  ArchLittle -> lift (unArch little)
  ArchBig -> lift (unArch big)


{- Quick lens implementation for handling state in FitParser -}

view :: Getter s a -> s -> a
view l x = getConst $ l Const x

over :: Setter s a -> (a -> a) -> s -> s
over l f x = runIdentity $ l (Identity . f) x

set :: Setter s a -> a -> s -> s
set l x = over l (const x)

use :: (Monad m, Functor m) => Getter s a -> StateT s m a
use l = fmap (view l) get

assign :: (Monad m, Functor m) => Setter s a -> a -> StateT s m ()
assign l x = modify (set l x)

(.=) :: (Monad m, Functor m) => Setter s a -> a -> StateT s m ()
l .= x = assign l x

(%=) :: (Monad m, Functor m) => Setter s a -> (a -> a) -> StateT s m ()
l %= f = modify (over l f)

type Lens f s a = (a -> f a) -> s -> f s

type Getter s a = Lens (Const a) s a

type Setter s a = Lens Identity s a

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
