-- | 
-- Module: Threshold
-- Description: Time integrated threshold functions
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Time integrated threshold functions typically used in condition monitoring.
module Language.Atom.Common.Threshold
  ( boolThreshold
  , doubleThreshold
  ) where

import Language.Atom.Expressions
import Language.Atom.Language
import Data.Int (Int32)
  
-- | Boolean thresholding over time.  Output is set when internal counter hits
-- limit, and cleared when counter is 0.
boolThreshold :: Name -> Int32 -> Bool -> E Bool -> Atom (E Bool)
boolThreshold name_ num init_ input = atom name_ $ do
  --assert "positiveNumber" $ num >= 0

  state <- bool "state" init_
  count <- int32 "count" (if init_ then num else 0)

  atom "update" $ do
    cond $ value count >. Const 0 &&. value count <. Const num
    count <== value count + mux input (Const 1) (Const (-1))

  atom "low" $ do
    cond $ value count ==. Const 0
    state <== false

  atom "high" $ do
    cond $ value count ==. Const num
    state <== true

  return $ value state

-- | Integrating threshold.  Output is set with integral reaches limit, and
-- cleared when integral reaches 0.
doubleThreshold :: Name -> Double -> E Double -> Atom (E Bool)
doubleThreshold name_ lim input = atom name_ $ do
  --assert "positiveLimit" $ lim >= 0

  state <- bool "state" False
  sum_ <- double "sum" 0

  -- TODO: Figure out what the below translates to in the newer library
  -- (high,low) <- priority

  atom "update" $ do
    sum_ <== value sum_ + input
    -- low

  atom "clear" $ do
    cond $ value sum_ <=. 0
    state <== false
    sum_ <== 0
    -- high

  atom "set" $ do
    cond $ value sum_ >=. Const lim
    state <== true
    sum_ <== Const lim
    -- high

  return  $ value state
