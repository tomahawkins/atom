-- | Fades one signal to another.
module Language.Atom.Common.Fader
  ( Fader
  , FaderInit (..)
  , fader
  , fadeToA
  , fadeToB
  , fadeToCenter
  ) where

import Language.Atom

-- | Fader object.
data Fader = Fader (V Int)

-- | Fader initalization.
data FaderInit = OnA | OnB | OnCenter

toA = 0
toB = 1
toCenter = 2

-- | Fader construction.  Name, fade rate, fader init, and signal A and B.
fader :: Name -> Double -> FaderInit -> E Double -> E Double -> Atom (Fader, E Double)
fader name rate init a b = atom name $ do
  --assert "positiveRate" $ rate >= 0

  target <- int (case init of {OnA -> toA; OnB -> toB; OnCenter -> toCenter})
  perA <- double (case init of {OnA -> 1;   OnB -> 0;   OnCenter -> 0.5})

  atom "toA" $ do
    cond $ value target ==. intC toA
    cond $ value perA <. 1
    perA <== mux (1 - value perA <. doubleC rate) 1 (value perA + doubleC rate)

  atom "toB" $ do
    cond $ value target ==. intC toB
    cond $ value perA >. 0
    perA <== mux (value perA <. doubleC rate) 0 (value perA - doubleC rate)

  atom "toCenterFrom0" $ do
    cond $ value target ==. intC toCenter
    cond $ value perA <. 0.5
    perA <== mux (0.5 - value perA <. doubleC rate) 0.5 (value perA + doubleC rate)

  atom "toCenterFrom1" $ do
    cond $ value target ==. intC toCenter
    cond $ value perA >. 0.5
    perA <== mux (value perA - 0.5 <. doubleC rate) 0.5 (value perA - doubleC rate)

  return (Fader target, (a * value perA + b * (1 - value perA)) / 2)

-- | Fade to signal A.
fadeToA :: Fader -> Action ()
fadeToA (Fader target) = target <== intC toA

-- | Fade to signal B.
fadeToB :: Fader -> Action ()
fadeToB (Fader target) = target <== intC toB

-- | Fade to center, ie average of signal A and B.
fadeToCenter :: Fader -> Action ()
fadeToCenter (Fader target) = target <== intC toCenter

