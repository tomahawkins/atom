-- | 
-- Module: Unit
-- Description: Unit testing, coverage, reporting & debugging
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Unit testing, coverage, reporting & debugging for Atom

module Language.Atom.Unit
  (
  -- * Types and Classes
    Test (..)
  , defaultTest
  , Random (..)
  -- * Test Execution
  , runTests
  -- * Printing Utilities
  , printStrLn
  , printIntegralE
  , printFloatingE
  , printProbe
  ) where

import Control.Monad
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Language.Atom.Code hiding (err)
import Language.Atom.Compile
import Language.Atom.Language
import System.Exit
import System.IO
import System.Process
import Text.Printf

import Prelude hiding (id)

-- | Parameters for a test simulation
data Test = Test
  { name      :: String -- ^ Name for test (used in log file, state name,
                 -- and output)
  , cycles    :: Int -- ^ Number of simulation cycles to run
  , testbench :: Atom () -- ^ Atom specification to test
  , modules   :: [FilePath] -- ^ Other C files to build in
  , includes  :: [FilePath] -- ^ Other C headers to include
  , declCode  :: String -- ^ C declarations and definitions inserted prior to
                 -- the @main@ function
  , initCode  :: String -- ^ C code executed inside the @main@ function, prior
                 -- to 'loopCode'
  , loopCode  :: String -- ^ C code executed inside the loop running the test
  , endCode   :: String -- ^ C code executed after that loop, but still in
                 -- @main@
  }

-- | Default test parameters
defaultTest :: Test
defaultTest = Test
  { name      = "test"
  , cycles    = 1000
  , testbench = return ()
  , modules   = []
  , includes  = []
  , declCode  = ""
  , initCode  = ""
  , loopCode  = ""
  , endCode   = ""
  }

-- | Run a list of tests, output a report on test passes and coverage.
runTests :: Int -- ^ Random seed
            -> [IO Test] -- ^ List of tests to run
            -> IO ()
runTests seed tests = do
  testResults <- mapM (runTest seed) tests
  let totalTests    = length testResults
      passingTests  = length $ filter (\ (_, p, _, _, _) -> p) testResults
      totalCoverage = nub $ concat [ a | (_, _, _, a, _) <- testResults ]
      unHitCoverage = sort $
                      totalCoverage \\
                      (nub $ concat [ a | (_, _, _, _, a) <- testResults ])
      totalCycles   = sum [ c | (_, _, c, _, _) <- testResults ]
      maxNameLen    = maximum [ length n | (n, _, _, _, _) <- testResults ]
  mapM_ (reportResult maxNameLen) testResults
  putStrLn ""
  putStrLn $ "Total Passing Tests     : " ++
    show passingTests ++ " / " ++ show totalTests
  putStrLn $ "Total Simulation Cycles : " ++
    show totalCycles
  putStrLn $ "Total Function Coverage : " ++
    show (length totalCoverage - length unHitCoverage) ++ " / " ++
    show (length totalCoverage)
  when (not $ null unHitCoverage) $ do
    putStrLn ""
    putStrLn "  Missed Coverage Points:"
    putStrLn ""
    mapM_ (putStrLn . ("    " ++)) unHitCoverage
  putStrLn ""
  putStrLn $ (if passingTests /= totalTests
              then "RED"
              else if not $ null unHitCoverage
                   then "YELLOW" else "GREEN") ++ " LIGHT"
  putStrLn ""
  when (passingTests /= totalTests) $ exitWith $ ExitFailure 2
  when (not $ null unHitCoverage)   $ exitWith $ ExitFailure 1

-- | Report results on a particular test
reportResult :: Int -- ^ Max name length
                -> (Name, Bool, Int, a, b) -- ^ (name, pass, cycles, _, _)
                -> IO ()
reportResult m (name', pass, cycles', _, _) =
      printf "%s:  %s    cycles = %7i  %s\n"
        (if pass then "pass" else "FAIL")
        (printf ("%-" ++ show m ++ "s") name' :: String)
        cycles'
        (if pass then "" else "    (see " ++ name' ++ ".log)")

-- | Run a single test with the given random seed. This will generate code,
-- compile it with GCC, execute it, and collect its output.
runTest :: Int -- ^ Random seed
           -> IO Test -- ^ Test to run
           -- | (name, pass status, cycles run, all coverage names,
           -- names covered)
           -> IO (Name, Bool, Int, [Name], [Name])
runTest seed test' = do
  test <- test'
  let config = defaults { cStateName = name test
                        , cCode = prePostCode test
                        , cRuleCoverage = False }
  putStrLn $ "running test " ++ name test ++ " ..."
  hFlush stdout
  (_, _, _, coverageNames, _) <-
    compile "atom_unit_test" config $ testbench test
  (exit, out, err) <- readProcessWithExitCode "gcc"
                      (["-Wall", "-g", "-o", "atom_unit_test"] ++
                       [ "-I" ++ i | i <- includes test ] ++
                       modules test ++
                       ["atom_unit_test.c"]) ""
  let file = name test ++ ".log"
  case exit of 
    ExitFailure _ -> do
      writeFile file $ out ++ err
      return (name test, False, 0, coverageNames, [])
    ExitSuccess -> do
      log_ <- readProcess "./atom_unit_test" [] ""
      let pass = not $ elem "FAILURE:" $ words log_
          covered = [ words line !! 1 |
                      line <- lines log_, isPrefixOf "covered:" line ]
      writeFile file $ out ++ err ++ log_
      hFlush stdout
      return (name test, pass, cycles test, coverageNames, covered)
  where
  prePostCode test assertionNames coverageNames _ = (preCode, postCode)
    where
    preCode = unlines
      [ "#include <stdio.h>"
      , "#include <stdlib.h>"
      , "void assert (int id, unsigned char check, unsigned long long clock) {"
      , "  static unsigned char failed[" ++
        show (length assertionNames) ++
        "] = {" ++ intercalate "," (replicate (length assertionNames) "0") ++
        "};"
      , "  if (! check) {"
      , "    " ++ intercalate "\n    else "
        [ "if (id == " ++ show id ++
          ") { if (! failed[id]) { printf(\"ASSERTION FAILURE: " ++ name' ++
          " at time %lli\\n\", clock); failed[id] = 1; } }" |
          (name', id) <- zip assertionNames [0::Int ..] ]
      , "  }"
      , "}"
      , "void cover  (int id, unsigned char check, unsigned long long clock) {"
      , "  static unsigned char covered[" ++
        show (length coverageNames) ++ "] = {" ++
        intercalate "," (replicate (length coverageNames) "0") ++ "};"
      , "  if (check) {"
      , "    " ++ intercalate "\n    else "
        [ "if (id == " ++ show id ++
          ") { if (! covered[id]) { printf(\"covered: " ++ name' ++
          " at time %lli\\n\", clock); covered[id] = 1; } }" |
          (name', id) <- zip coverageNames [0::Int ..] ]
      , "  }"
      , "}"
      ] ++ declCode test

    postCode = unlines
      [ "int main() {"
      , "  int loop;"
      , "  srand(" ++ show seed ++ ");"
      , initCode test
      , "  for (loop = 0; loop < " ++ show (cycles test) ++ "; loop++) {"
      , "    atom_unit_test();"
      , loopCode test
      , "  }"
      , endCode test
      , "  return 0;"
      , "}"
      ]

-- | Print a string in C using @printf@, appending a newline.
printStrLn :: String -> Atom ()
printStrLn s = action (\ _ -> "printf(\"" ++ s ++ "\\n\")") []

-- | Print an integral value in C using @printf@.
printIntegralE :: IntegralE a =>
                  String -- ^ Prefix for printed value
                  -> E a -- ^ Integral value to print
                  -> Atom ()
printIntegralE name' value' = 
  action (\ v' -> "printf(\"" ++ name' ++ ": %i\\n\", " ++ head v' ++ ")")
  [ue value']

-- | Print a floating point value in C using @printf@.
printFloatingE :: FloatingE a =>
                  String -- ^ Prefix for printed value
                  -> E a -- ^ Floating point value to print
                  -> Atom ()
printFloatingE name' value' = 
  action (\ v' -> "printf(\"" ++ name' ++ ": %f\\n\", " ++ head v' ++ ")")
  [ue value']

-- TODO: Factor out common code in the above - which is all of it, except
-- for a single character (%i vs. %f)

-- | Print the value of a probe to the console (along with its name).
printProbe :: (String, UE) -> Atom ()
printProbe (str, ue_) = case typeOf ue_ of
  Bool   -> printIntegralE str (ruInt   :: E Int8)
  Int8   -> printIntegralE str (ruInt   :: E Int8)
  Int16  -> printIntegralE str (ruInt   :: E Int16)
  Int32  -> printIntegralE str (ruInt   :: E Int32)
  Int64  -> printIntegralE str (ruInt   :: E Int64)
  Word8  -> printIntegralE str (ruInt   :: E Word8)
  Word16 -> printIntegralE str (ruInt   :: E Word16)
  Word32 -> printIntegralE str (ruInt   :: E Word32)
  Word64 -> printIntegralE str (ruInt   :: E Word64)
  Double -> printFloatingE str (ruFloat :: E Double)
  Float  -> printFloatingE str (ruFloat :: E Float)
  where ruInt :: IntegralE a => E a
        ruInt = Retype ue_
        ruFloat :: FloatingE a => E a
        ruFloat = Retype ue_

class Expr a => Random a where
  random :: E a

instance Random Bool   where random = (1 .&. random32) ==. 1
instance Random Word8  where random = Cast random32
instance Random Word16 where random = Cast random32
instance Random Word32 where random = Cast random32
instance Random Word64 where
  random = Cast random32 .|. shiftL (Cast random32) 32
instance Random Int8   where random = Cast random32
instance Random Int16  where random = Cast random32
instance Random Int32  where random = Cast random32
instance Random Int64  where random = Cast (random :: E Word64)

random32 :: E Word32
random32 = value $ word32' "rand()"

