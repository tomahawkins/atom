-- | Atom compilation.
module Language.Atom.Compile
  ( compile
  , reportSchedule
  , Schedule
  ) where

import System.Exit

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
      return (schedule', ruleCoverage, assertionNames, coverageNames, probeNames)

