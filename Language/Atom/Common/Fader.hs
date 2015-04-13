-- | 
-- Module: Fader
-- Description: Fades one signal to another
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Fades one signal to another.

module Language.Atom.Common.Fader
  ( Fader
  , FaderInit (..)
  , fader
  , fadeToA
  , fadeToB
  , fadeToCenter
  ) where

import Language.Atom.Expressions
import Language.Atom.Language
import Data.Int (Int32)

-- | Fader object.
data Fader = Fader (V Int32)

-- | Fader initalization.
data FaderInit = OnA -- ^ Start at signal A
               | OnB -- ^ Start at signal B
               | OnCenter -- ^ Start at average of A and B

toA, toB, toCenter :: Int32
toA = 0
toB = 1
toCenter = 2

-- | Fader construction
fader :: Name -- ^ Name
         -> Double -- ^ Fade rate
         -> FaderInit -- ^ Initialization
         -> E Double -- ^ Signal A
         -> E Double -- ^ Signal B
         -> Atom (Fader, E Double)
fader name_ rate init_ a b = atom name_ $ do
  --assert "positiveRate" $ rate >= 0

  target <- int32 "target" $ case init_ of OnA -> toA
                                           OnB -> toB
                                           OnCenter -> toCenter
  perA <- double "perA" $ case init_ of OnA -> 1
                                        OnB -> 0
                                        OnCenter -> 0.5

  atom "toA" $ do
    cond $ value target ==. Const toA
    cond $ value perA <. 1
    perA <== mux (1 - value perA <. Const rate) 1 (value perA + Const rate)

  atom "toB" $ do
    cond $ value target ==. Const toB
    cond $ value perA >. 0
    perA <== mux (value perA <. Const rate) 0 (value perA - Const rate)

  atom "toCenterFrom0" $ do
    cond $ value target ==. Const toCenter
    cond $ value perA <. 0.5
    perA <== mux (0.5 - value perA <. Const rate) 0.5 (value perA + Const rate)

  atom "toCenterFrom1" $ do
    cond $ value target ==. Const toCenter
    cond $ value perA >. 0.5
    perA <== mux (value perA - 0.5 <. Const rate) 0.5 (value perA - Const rate)

  return (Fader target, (a * value perA + b * (1 - value perA)) / 2)

-- | Fade to signal A.
fadeToA :: Fader -> Atom ()
fadeToA (Fader target) = target <== Const toA

-- | Fade to signal B.
fadeToB :: Fader -> Atom ()
fadeToB (Fader target) = target <== Const toB

-- | Fade to center, i.e. average of signal A and B.
fadeToCenter :: Fader -> Atom ()
fadeToCenter (Fader target) = target <== Const toCenter
