-- | 
-- Module: Common
-- Description: Common functions.
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Common Atom functions

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
  -- * Channels
  , Channel (..)
  , channel
  , writeChannel
  , readChannel
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

-- | 'True' when a timer has completed. Note that this remains 'True' until
-- the timer is restarted.
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

-- | 1-D lookup table.  @x@ values out of table range are clipped at end @y@
-- values.  Input table must be monotonically increasing in @x@.
lookupTable :: FloatingE a => [(E a, E a)] -- ^ (@x@, @y@) lookup table
               -> E a -- ^ Input @x@ value
               -> E a -- ^ Output @y@ value
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
-- The two @x@ points must be different to prevent a divide-by-zero.
linear :: FloatingE a => (E a, E a) -- ^ First point, (x1, y1)
          -> (E a, E a) -- ^ Second point, (x2, y2)
          -> E a -- ^ Input @x@ value
          -> E a -- ^ Interpolated/extrapolated @y@ value
linear (x1, y1) (x2, y2) a = slope * a + inter
  where
  slope = (y2 - y1) / (x2 - x1)
  inter = y1 - slope * x1

-- | Hysteresis returns 'True' when the input exceeds @max@ and 'False' when
-- the input is less than @min@.  The state is held when the input is between
-- @min@ and @max@.
hysteresis :: OrdE a => E a -- ^ min
              -> E a -- ^ max
              -> E a -- ^ Input
              -> Atom (E Bool)
hysteresis a b u = do
  s <- bool "s" False
  s <== (mux (u >. max') true $ mux (u <. min') false $ value s)
  return $ value s
  where
  min' = min_ a b
  max' = max_ a b

-- | A channel is a uni-directional communication link that ensures one read
-- for every write.
data Channel a = Channel a (V Bool)

-- | Creates a new channel, with a given name and data.
channel :: a -> Atom (Channel a)
channel a = do
  hasData <- bool "hasData" False
  return $ Channel a hasData

-- | Write data to a 'Channel'.  A write will only suceed if the 'Channel' is
-- empty.
writeChannel :: Channel a -> Atom ()
writeChannel (Channel _ hasData) = do
  cond $ not_ $ value hasData
  hasData <== true

-- | Read data from a 'Channel'.  A read will only suceed if the 'Channel' has
-- data to be read.
readChannel :: Channel a -> Atom a
readChannel (Channel a hasData) = do
  cond $ value hasData
  hasData <== false
  return a

{-
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

-}
