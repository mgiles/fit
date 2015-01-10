{-|
Module      : Fit.Internal.Numbers
Copyright   : Copyright 2014-2015, Matt Giles
License     : Modified BSD License (see LICENSE file)
Maintainer  : matt.w.giles@gmail.com
Stability   : experimental

Little-endian and big-endian parsers for signed and unsigned integers, and
for single-precision and double-precision floating point numbers.

The parsers are tagged for endianness using the tools in "Fit.Internal.Architecture".
-}

module Fit.Internal.Numbers (
  -- * Little-endian parsers
  int16le,
  int32le,
  int64le,
  word16le,
  word32le,
  word64le,
  float32le,
  float64le,

  -- * Big-endian parsers
  int16be,
  int32be,
  int64be,
  word16be,
  word32be,
  word64be,
  float32be,
  float64be,

  -- * Helpers
  nByteIntLe,
  nByteIntBe
  ) where

import Fit.Internal.Architecture

import Control.Applicative ((<$>))

import Data.Int (Int16, Int32, Int64)
import Data.Bits (Bits, shiftL, FiniteBits, finiteBitSize)
import Data.Word (Word16, Word32, Word64)

import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A (take)
import qualified Data.ByteString as B (foldr, foldl')

import qualified Foreign as F (alloca, poke, peek, castPtr)
import System.IO.Unsafe (unsafePerformIO)

{- little-endian parsers -}
word16le :: LittleEndian (Parser Word16)
word16le = parseLe

word32le :: LittleEndian (Parser Word32)
word32le = parseLe

word64le :: LittleEndian (Parser Word64)
word64le = parseLe

int16le :: LittleEndian (Parser Int16)
int16le = parseLe

int32le :: LittleEndian (Parser Int32)
int32le = parseLe

int64le :: LittleEndian (Parser Int64)
int64le = parseLe

parseLe :: (Integral a, FiniteBits a) => LittleEndian (Parser a)
parseLe = withLE $ p 0
  where p :: (Integral a, FiniteBits a) => a -> Parser a
        p proxy = let n = byteSize proxy
                  in nByteIntLe n

float32le :: LittleEndian (Parser Float)
float32le = fmap (fmap toFloat) word32le

float64le :: LittleEndian (Parser Double)
float64le = fmap (fmap toDouble) word64le

-- | Parse @n@ bytes and interpret them as a little-endian integer. The caller
-- must ensure that the returned type is the correct size for the number of
-- bytes parsed.
nByteIntLe :: (Integral a, Bits a) => Int -> Parser a
nByteIntLe nbytes = B.foldr go 0 <$> A.take nbytes
  where go b acc = (acc `shiftL` 8) + (fromIntegral b)

{- big-endian parsers -}
word16be :: BigEndian (Parser Word16)
word16be = parseBe

word32be :: BigEndian (Parser Word32)
word32be = parseBe

word64be :: BigEndian (Parser Word64)
word64be = parseBe

int16be :: BigEndian (Parser Int16)
int16be = parseBe

int32be :: BigEndian (Parser Int32)
int32be = parseBe

int64be :: BigEndian (Parser Int64)
int64be = parseBe

parseBe :: (Integral a, FiniteBits a) => BigEndian (Parser a)
parseBe = withBE $ p 0
  where p :: (Integral a, FiniteBits a) => a -> Parser a
        p proxy = let n = byteSize proxy
                  in nByteIntBe n

float32be :: BigEndian (Parser Float)
float32be = fmap (fmap toFloat) word32be

float64be :: BigEndian (Parser Double)
float64be = fmap (fmap toDouble) word64be

-- | Parse @n@ bytes and interpret them as a big-endian integer. The caller
-- must ensure that the returned type is the correct size for the number of
-- bytes parsed.
nByteIntBe :: (Integral a, Bits a) => Int -> Parser a
nByteIntBe nbytes = B.foldl' go 0 <$> A.take nbytes
  where go acc b = (acc `shiftL` 8) + (fromIntegral b)

-- | Get the number of 8-bit bytes used to represent the given number.
-- The actual value passed is not used at all.
byteSize :: (FiniteBits a) => a -> Int
byteSize n = finiteBitSize n `div` 8

{-
  Conversion helpers for re-interpreting bytes as floating-point numbers.
  See this Stack Overflow post for details on word -> float conversion:

  https://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-float/7002812
-}

toFloat :: Word32 -> Float
toFloat word = unsafePerformIO $ F.alloca $ \buf -> do
  F.poke (F.castPtr buf) word
  F.peek buf

toDouble :: Word64 -> Double
toDouble word = unsafePerformIO $ F.alloca $ \buf -> do
  F.poke (F.castPtr buf) word
  F.peek buf
