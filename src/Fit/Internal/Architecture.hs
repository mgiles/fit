{-|
Module      : Fit.Internal.Architecture
Copyright   : Copyright 2014-2015, Matt Giles
License     : Modified BSD License (see LICENSE file)
Maintainer  : matt.w.giles@gmail.com
Stability   : experimental

FIT files can contain values encoded with both little-endian and big-endian orderings, and this
can vary throughout the file. This module provides some helper types for using parsers
with the correct endian-ness during the parse. See the "Fit.Internal.Numbers" and
"Fit.Internal.FitParser" modules for example use.
-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Fit.Internal.Architecture (
  Arch(..),
  WithArch(..),
  LittleEndian,
  BigEndian,
  withLE,
  withBE
  ) where

import Control.Applicative (Applicative, pure, (<*>))

data Arch = ArchLittle
          | ArchBig
          deriving (Show, Eq)

-- | Tag a value for use in a little- or big-endian context (with 'ArchLittle' and 'ArchBig', respectively)
newtype WithArch (a :: Arch) b = WithArch { unArch :: b }

instance Functor (WithArch a) where
  fmap f (WithArch x) = WithArch (f x)

instance Applicative (WithArch a) where
  pure = WithArch
  (WithArch f) <*> (WithArch x) = WithArch (f x)

-- | Convenience type for values to use in a little-endian context
type LittleEndian a = WithArch ArchLittle a

-- | Convenience type for values to use in a big-endian context
type BigEndian a = WithArch ArchBig a

-- | Alias for 'pure'
withLE :: a -> LittleEndian a
withLE = pure

-- | Alias for 'pure'
withBE :: a -> BigEndian a
withBE = pure
