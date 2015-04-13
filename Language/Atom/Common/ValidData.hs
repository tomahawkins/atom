-- | 
-- Module: ValidData
-- Description: Capturing data that can either be valid or invalid
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Capturing data that can either be valid or invalid.
module Language.Atom.Common.ValidData
  ( ValidData
  , validData
  , getValidData
  , whenValid
  , whenInvalid
  ) where

import Language.Atom.Expressions
import Language.Atom.Language

-- | 'ValidData' captures the data and its validity condition.
--   'ValidData' is abstract to prevent rules from using invalid data.
data ValidData a = ValidData a (E Bool)

-- | Create 'ValidData' given the data and validity condition.
validData :: a -> E Bool -> ValidData a
validData = ValidData

-- | Get a valid data.  Action is disabled if data is invalid.
getValidData :: ValidData a -> Atom a
getValidData (ValidData a v) = cond v >> return a

-- | Action enabled if 'ValidData' is valid.
whenValid :: ValidData a -> Atom ()
whenValid (ValidData _ v) = cond v

-- | Action enabled if 'ValidData' is not valid.
whenInvalid :: ValidData a -> Atom ()
whenInvalid (ValidData _ v) = cond $ not_ v
