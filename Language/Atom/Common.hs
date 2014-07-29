-- | Common Atom functions.
module Language.Atom.Common
  (
  -- * Timers
    Timer
  , timer
  , startTimer
  , startTimerIf
  , timerDone
  -- * One Shots
  , oneShotRise
  , oneShotFall
  -- * Debouncing
  , debounce
  -- * Lookup Tables
  , lookupTable
  , linear
  -- * Hysteresis
  , hysteresis
  ) where

import Data.Word

import Language.Atom.Language

-- | A Timer.
data Timer = Timer (V Word64)

-- | Creates a new timer.
timer :: Name -> Atom Timer
timer name = do
  timer' <- word64 name 0
  return $ Timer timer'

-- | Starts a Timer.  A timer can be restarted at any time.
startTimer :: Timer -- ^ Timer to start
           -> E Word64 -- ^ Number of clock ticks the timer shall run
           -> Atom ()
startTimer t = startTimerIf t true

-- | Conditionally start a Timer.
startTimerIf :: Timer -- ^ Timer to start conditionally
             -> E Bool -- ^ Condition for starting the timer
             -> E Word64 -- ^ Number of ticks the timer shall run
             -> Atom ()
startTimerIf (Timer t) a time = t <== mux a (clock + time) (value t)

-- | 'True' when a timer has completed.
timerDone :: Timer -> E Bool
timerDone (Timer t) = value t <=. clock


-- | One-shot on a rising transition.
oneShotRise :: E Bool -> Atom (E Bool)
oneShotRise a = do
  last' <- bool "last" False
  last' <== a
  return $ a &&. not_ (value last')

-- | One-shot on a falling transition.
oneShotFall :: E Bool -> Atom (E Bool)
oneShotFall = oneShotRise . not_


-- | Debounces a boolean given an on and off time (ticks) and an initial state.
debounce :: Name -- ^ Name of the resulting atom
         -> E Word64 -- ^ On time in ticks
         -> E Word64 -- ^ Off time in ticks
         -> Bool -- ^ Initial value
         -> E Bool -- ^ The boolean to debounce
         -> Atom (E Bool) -- ^ Resulting debounced boolean
debounce name onTime offTime init' a = atom name $ do
  lst  <- bool "last" init'
  out   <- bool "out"  init'
  timer' <- timer "timer"
  atom "on" $ do
    cond $ a &&. not_ (value lst)
    startTimer timer' onTime
    lst <== a
  atom "off" $ do
    cond $ not_ a &&. value lst
    startTimer timer' offTime
    lst <== a
  atom "set" $ do
    cond $ a ==. value lst
    cond $ timerDone timer'
    out <== value lst
  return $ value out


-- | 1-D lookup table.  X values out of table range are clipped at end Y values.
--   Input table must be monotonically increasing in X.
lookupTable :: FloatingE a => [(E a, E a)] -> E a -> E a
lookupTable table x = mux (x >=. x1) y1 $ foldl f y0 table'
  where
  (_,  y0) = head table
  (x1, y1) = last table
  table' = zip (init table) (tail table)
  f a ((a0,b0),(a1,b1)) = mux (x >=. a0) interp a
    where
    slope = (b1 - b0) / (a1 - a0)
    interp = (x - a0) * slope + b0

-- | Linear extrapolation and interpolation on a line with 2 points.
--   The two x points must be different to prevent a divide-by-zero.
linear :: FloatingE a => (E a, E a) -> (E a, E a) -> E a -> E a
linear (x1, y1) (x2, y2) a = slope * a + inter
  where
  slope = (y2 - y1) / (x2 - x1)
  inter = y1 - slope * x1

-- | Hysteresis returns 'True' when the input exceeds @max@ and 'False' when
--   the input is less than @min@.  The state is held when the input is between
--   @min@ and @max@.
--
-- > hysteresis name min max input
hysteresis :: OrdE a => E a -> E a -> E a -> Atom (E Bool)
hysteresis a b u = do
  s <- bool "s" False
  s <== (mux (u >. max') true $ mux (u <. min') false $ value s)
  return $ value s
  where
  min' = min_ a b
  max' = max_ a b

{-

-- | A channel is a uni-directional communication link that ensures one read for every write.
data Channel a = Channel a (V Bool)

-- | Creates a new channel, with a given name and data.
channel :: a -> Atom (Channel a)
channel a = do
  hasData <- bool False
  return $ Channel a hasData

-- | Write data to a 'Channel'.  A write will only suceed if the 'Channel' is empty.
writeChannel :: Channel a -> Action ()
writeChannel (Channel _ hasData) = do
  when $ not_ $ value hasData
  hasData <== true

-- | Read data from a 'Channel'.  A read will only suceed if the 'Channel' has data to be read.
readChannel :: Channel a -> Action a
readChannel (Channel a hasData) = do
  when $ value hasData
  hasData <== false
  return a

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
fader name rate init a b = scope name $ do
  --assert "positiveRate" $ rate >= 0

  target <- int (case init of {OnA -> toA; OnB -> toB; OnCenter -> toCenter})
  perA <- double (case init of {OnA -> 1;   OnB -> 0;   OnCenter -> 0.5})

  rule "toA" $ do
    when $ value target ==. intC toA
    when $ value perA <. 1
    perA <== mux (1 - value perA <. doubleC rate) 1 (value perA + doubleC rate)

  rule "toB" $ do
    when $ value target ==. intC toB
    when $ value perA >. 0
    perA <== mux (value perA <. doubleC rate) 0 (value perA - doubleC rate)

  rule "toCenterFrom0" $ do
    when $ value target ==. intC toCenter
    when $ value perA <. 0.5
    perA <== mux (0.5 - value perA <. doubleC rate) 0.5 (value perA + doubleC rate)

  rule "toCenterFrom1" $ do
    when $ value target ==. intC toCenter
    when $ value perA >. 0.5
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

module Language.Atom.Common.Process
  ( Process (..)
  , process
  ) where

import Language.Atom

data Process
  = Par [Process]
  | Seq [Process]
  | Alt [Process]
  | Rep Process
  | Act Action

process :: Name -> Process -> Atom ()

-- | Time integrated threshold functions typically used in condition monitoring.
module Language.Atom.Common.Threshold
  ( boolThreshold
  , floatingThreshold
  ) where

import Language.Atom


-- | Boolean thresholding over time.  Output is set when internal counter hits limit, and cleared when counter is 0.
boolThreshold :: Name -> Int -> Bool -> E Bool -> Atom (E Bool)
boolThreshold name num init input = scope name $ do
  --assert "positiveNumber" $ num >= 0

  state <- bool init
  count <- int  (if init then num else 0)

  rule "update" $ do
    when $ value count >. 0 &&. value count <. num
    count <== value count + mux input 1 (-1)

  rule "low" $ do
    when $ value count ==. 0
    state <== false

  rule "high" $ do
    when $ value count ==. intC num
    state <== true

  return $ value state

-- | Integrating threshold.  Output is set with integral reaches limit, and cleared when integral reaches 0.
doubleThreshold :: Name -> Double -> E Double -> Atom (E Bool)
doubleThreshold name lim input = scope name $ do
  --assert "positiveLimit" $ lim >= 0

  state <- bool False
  sum <- double 0

  (high,low) <- priority

  rule "update"
    sum <== value sum + input
    low

  rule "clear" $ do
    when $ value sum <=. 0
    state <== false
    sum <== 0
    high

  rule "set" $ do
    when $ value sum >=. doubleC lim
    state <== true
    sum <== doubleC lim
    high

  return  $ value state

-- | Capturing data that can either be valid or invalid.
module Language.Atom.Common.ValidData
  ( ValidData
  , validData
  , getValidData
  , whenValid
  , whenInvalid
  ) where

import Language.Atom

-- | 'ValidData' captures the data and its validity condition.
--   'ValidData' is abstract to prevent rules from using invalid data.
data ValidData a = ValidData a (E Bool)

-- | Create 'ValidData' given the data and validity condition.
validData :: a -> E Bool -> ValidData a
validData = ValidData

-- | Get a valid data.  Action is disabled if data is invalid.
getValidData :: ValidData a -> Action a
getValidData (ValidData a v) = cond v >> return a

-- | Action enabled if 'ValidData' is valid.
whenValid :: ValidData a -> Action ()
whenValid (ValidData _ v) = cond v

-- | Action enabled if 'ValidData' is not valid.
whenInvalid :: ValidData a -> Action ()
whenInvalid (ValidData _ v) = cond $ not_ v
-}
