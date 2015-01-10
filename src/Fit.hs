{-|
Module      : Fit
Copyright   : Copyright 2014-2015, Matt Giles
License     : Modified BSD License
Maintainer  : matt.w.giles@gmail.com
Stability   : experimental
-}

module Fit (
  -- * Messages API
  -- $messages
  readMessages,
  readFileMessages,
  parseMessages,
  Messages(..),
  Message(..),
  Field(..),
  Value(..),
  SingletonValue(..),
  ArrayValue(..),

  -- ** Lenses for the Messages API
  messages,
  message,
  messageNumber,
  fields,
  field,
  fieldNumber,
  fieldValue,
  int,
  real,
  text,
  byte,
  ints,
  reals,
  bytestring
  ) where

import Fit.Messages
import Fit.Messages.Lens

-- $messages
-- A high-level view of a FIT file as a sequence of data messages. This is the
-- recommended API for pulling information out of a FIT file, but if you need
-- access to the exact structure of the file you can use the API in "Fit.Internal.FitFile" and "Fit.Internal.Parse".
--
-- Some basic lenses are also provided for working with the Messages API. These
-- can make it much easier to extract information from a FIT file, especially
-- since you usually know what sort of data you're expecting to find.
--
-- For example, from the FIT global profile we can find that the global message
-- number for 'record' messages is 20, and within a `record` message the 'speed'
-- field has field number 6. The following code gets the `speed` field from all
-- 'record' messages in the file:
--
-- @
-- Right fit <- readFileMessages "file.fit"
-- let speeds = fit ^.. message 20 . field 6 . int
-- -- speeds :: [Int]
-- @
--
-- Note that this package doesn't provide any lens combinators (like @(^..)@),
-- so you'll need to use ones from a lens package.
