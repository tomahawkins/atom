-- | Atom compilation.
module Language.Atom.Compile
  ( compile
  , reportSchedule
  , Schedule
  ) where

import System.Exit
import Control.Monad (when)
import Data.Maybe (isJust)

import Language.Atom.Code
import Language.Atom.Scheduling
import Language.Atom.Elaboration
import Language.Atom.Language hiding (Atom)

-- | Compiles an atom description to C.
compile :: Name -> Config -> Atom () -> IO (Schedule, RuleCoverage, [Name], [Name], [(Name, Type)])
compile name config atom = do
  r <- elaborate name atom
  case r of
    Nothing -> putStrLn "ERROR: Design rule checks failed." >> exitWith (ExitFailure 1)
    Just (state, rules, assertionNames, coverageNames, probeNames) -> do
      let schedule' = schedule rules
      ruleCoverage <- writeC name config state rules schedule' assertionNames coverageNames probeNames
      when (isJust $ hardwareClock config) (putStrLn hwClockWarning)
      return (schedule', ruleCoverage, assertionNames, coverageNames, probeNames)

hwClockWarning :: String
hwClockWarning = unlines
 [ ""
 , "*** Atom WARNING: you are configuring to use a harware clock.  Please remember to assign" 
 , "    the current time (accoring to your clockName field in Clock) the first time you"
 , "    enter the main Atom-generated function calling your rules."
 , ""
 ]
