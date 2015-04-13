{- |
Module: Atom
Description: Top-level Atom module
Copyright: (c) 2013 Tom Hawkins & Lee Pike

Atom is a Haskell DSL for designing hard realtime embedded software.

Based on guarded atomic actions (similar to STM), Atom enables highly
concurrent programming without the need for mutex locking.
In addition, Atom performs compile-time task scheduling and generates code
with deterministic execution time and constant memory use, simplifying the
process of timing verification and memory consumption in hard realtime
applications. Without mutex locking and run-time task scheduling,
Atom eliminates the need and overhead of RTOSes for many embedded applications.
-}

module Language.Atom
  ( -- * Code 
    -- | Module: "Language.Atom.Code" 
    Config (..), defaults, Clock (..), defaultClock, writeC, cType, RuleCoverage,
    -- * Compilation
    -- | Module: "Language.Atom.Compile"
    compile, reportSchedule, Schedule,
    -- * Common
    -- | Module: "Language.Atom.Common"
    Timer, timer, startTimer, startTimerIf, timerDone, oneShotRise,
    oneShotFall, debounce, lookupTable, linear, hysteresis, Channel (..),
    channel, writeChannel, readChannel,
    -- ** Signal fading
    -- | Module: "Language.Atom.Common.Fader"
    Fader, FaderInit (..), fader, fadeToA, fadeToB, fadeToCenter,
    -- ** Thresholds
    -- | Module: "Language.Atom.Common.Threshold"
    boolThreshold, doubleThreshold,
    -- ** Valid/Invalid data
    -- | Module: "Language.Atom.Common.ValidData"
    ValidData, validData, getValidData, whenValid, whenInvalid,
    -- * Language & EDSL
    -- | Module: "Language.Atom.Language"
    Atom, atom, period, getPeriod, phase, exactPhase, getPhase, cond,
    Assign (..), incr, decr, var, var', array, array', bool, bool', int8,
    int8', int16, int16', int32, int32', int64, int64', word8, word8', word16,
    word16', word32, word32', word64, word64', float, float', double, double',
    action, call, probe, probes, assert, cover, assertImply, Name, liftIO,
    path, clock, nextCoverage,
    -- * Expressions
    -- | Module: "Language.Atom.Expressions"
    E(..), V(..), UE(..), UV(..), A(..), UA(..), Expr(..), Expression(..),
    Variable(..), Type(..), Const(..), Width(..), TypeOf(..), bytes, ue, uv,
    NumE, IntegralE, FloatingE, EqE, OrdE, true, false,
    value, not_, (&&.), (||.), and_, or_, any_, all_, imply, (.&.), complement,
    (.|.), xor, (.<<.), (.>>.), rol, ror, bitSize, isSigned, (==.), (/=.),
    (<.), (<=.), (>.), (>=.), min_, minimum_, max_, maximum_, limit, div_,
    div0_, mod_, mod0_, mux, (!), (!.), ubool, unot, uand, uor, ueq, umux,
    -- * Testing
    -- | Module: "Language.Atom.Unit"
    Test(..), defaultTest, Random (..), runTests, printStrLn, printIntegralE,
    printFloatingE, printProbe
  ) where

import Language.Atom.Code
import Language.Atom.Compile
import Language.Atom.Common
import Language.Atom.Common.Fader
import Language.Atom.Common.Threshold
import Language.Atom.Common.ValidData
import Language.Atom.Language
import Language.Atom.Unit
