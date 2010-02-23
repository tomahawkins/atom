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
  ) where

import Control.Monad
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Language.Atom.Code
import Language.Atom.Compile
import Language.Atom.Language
import System.Exit
import System.IO
import System.Process
import Text.Printf


-- | Data constructor:Test
data Test = Test
  { name      :: String
  , cycles    :: Int
  , testbench :: Atom ()
  , modules   :: [FilePath]
  , includes  :: [FilePath]
  , declCode  :: String
  , initCode  :: String
  , loopCode  :: String
  , endCode   :: String
  }

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


-- | Running TestList
runTests :: Int -> [IO Test] -> IO ()
runTests seed tests = do
  testResults <- mapM (runTest seed) tests
  let totalTests    = length testResults
      passingTests  = length $ filter (\ (_, p, _, _, _) -> p) testResults
      totalCoverage = nub $ concat [ a | (_, _, _, a, _) <- testResults ]
      unHitCoverage = sort $ totalCoverage \\ (nub $ concat [ a | (_, _, _, _, a) <- testResults ])
      totalCycles   = sum [ c | (_, _, c, _, _) <- testResults ]
      maxNameLen    = maximum [ length n | (n, _, _, _, _) <- testResults ]
  mapM_ (reportResult maxNameLen) testResults
  putStrLn ""
  putStrLn $ "Total Passing Tests     : " ++ show passingTests ++ " / " ++ show totalTests
  putStrLn $ "Total Simulation Cycles : " ++ show totalCycles
  putStrLn $ "Total Function Coverage : " ++ show (length totalCoverage - length unHitCoverage) ++ " / " ++ show (length totalCoverage)
  when (not $ null unHitCoverage) $ do
    putStrLn ""
    putStrLn "  Missed Coverage Points:"
    putStrLn ""
    mapM_ (putStrLn . ("    " ++)) unHitCoverage
  putStrLn ""
  putStrLn $ (if passingTests /= totalTests then "RED" else if not $ null unHitCoverage then "YELLOW" else "GREEN") ++ " LIGHT"
  putStrLn ""
  when (passingTests /= totalTests) $ exitWith $ ExitFailure 2
  when (not $ null unHitCoverage)   $ exitWith $ ExitFailure 1

reportResult :: Int -> (Name, Bool, Int, a, b) -> IO ()
reportResult m (name, pass, cycles, _, _) =
      printf "%s:  %s    cycles = %7i  %s\n"
        (if pass then "pass" else "FAIL")
        (printf ("%-" ++ show m ++ "s") name :: String)
        cycles
        (if pass then "" else "    (see " ++ name ++ ".log)")

runTest :: Int -> IO Test -> IO (Name, Bool, Int, [Name], [Name])
runTest seed test = do
  test <- test
  putStrLn $ "running test " ++ name test ++ " ..."
  hFlush stdout
  (_, _, _, coverageNames, _) <- compile "atom_unit_test" defaults { cCode = prePostCode test, cRuleCoverage = False } $ testbench test
  (exit, out, err) <- readProcessWithExitCode "gcc" (["-Wall", "-g", "-o", "atom_unit_test"] ++ [ "-i" ++ i | i <- includes test ] ++ modules test ++ ["atom_unit_test.c"]) ""
  let file = name test ++ ".log"
  case exit of 
    ExitFailure _ -> do
      writeFile file $ out ++ err
      return (name test, False, 0, coverageNames, [])
    ExitSuccess -> do
      log <- readProcess "./atom_unit_test" [] ""
      let pass = not $ elem "FAILURE:" $ words log
          covered = [ words line !! 1 | line <- lines log, isPrefixOf "covered:" line ]
      writeFile file $ out ++ err ++ log
      hFlush stdout
      return (name test, pass, cycles test, coverageNames, covered)
  where
  prePostCode test assertionNames coverageNames _ = (preCode, postCode)
    where
    preCode = unlines
      [ "#include <stdio.h>"
      , "#include <stdlib.h>"
      , "void assert (int id, unsigned char check, unsigned long long clock) {"
      , "  static unsigned char failed[" ++ show (length assertionNames) ++ "] = {" ++ intercalate "," (replicate (length assertionNames) "0") ++ "};"
      , "  if (! check) {"
      , "    " ++ intercalate "\n    else " [ "if (id == " ++ show id ++ ") { if (! failed[id]) { printf(\"ASSERTION FAILURE: " ++ name ++ " at time %lli\\n\", clock); failed[id] = 1; } }" | (name, id) <- zip assertionNames [0..] ]
      , "  }"
      , "}"
      , "void cover  (int id, unsigned char check, unsigned long long clock) {"
      , "  static unsigned char covered[" ++ show (length coverageNames) ++ "] = {" ++ intercalate "," (replicate (length coverageNames) "0") ++ "};"
      , "  if (check) {"
      , "    " ++ intercalate "\n    else " [ "if (id == " ++ show id ++ ") { if (! covered[id]) { printf(\"covered: " ++ name ++ " at time %lli\\n\", clock); covered[id] = 1; } }" | (name, id) <- zip coverageNames [0..] ]
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
  



-- | Printing strings in C using printf.
printStrLn :: String -> Atom ()
printStrLn s = action (\ _ -> "printf(\"" ++ s ++ "\\n\")") []

-- | Print integral values.
printIntegralE :: IntegralE a => String -> E a -> Atom ()
printIntegralE name value = action (\ [v] -> "printf(\"" ++ name ++ ": %i\\n\", " ++ v ++ ")") [ue value]

-- | Print floating point values.
printFloatingE :: FloatingE a => String -> E a -> Atom ()
printFloatingE name value = action (\ [v] -> "printf(\"" ++ name ++ ": %f\\n\", " ++ v ++ ")") [ue value]


class Expr a => Random a where random :: E a

instance Random Bool   where random = (1 .&. random32) ==. 1
instance Random Word8  where random = Cast random32
instance Random Word16 where random = Cast random32
instance Random Word32 where random = Cast random32
instance Random Word64 where random = Cast random32 .|. shiftL (Cast random32) 32
instance Random Int8   where random = Cast random32
instance Random Int16  where random = Cast random32
instance Random Int32  where random = Cast random32
instance Random Int64  where random = Cast (random :: E Word64)

random32 :: E Word32
random32 = value $ word32' "rand()"

