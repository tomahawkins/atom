-- | 
-- Module: Gcd
-- Description: Example design which computes GCD (greatest-common divisor)
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--

module Language.Atom.Example.Gcd
  ( compileExample
  , example
  ) where

import Language.Atom

-- | Invoke the Atom compiler
compileExample :: IO ()
compileExample = do
  (schedule, _, _, _, _) <- compile "example" defaults { cCode = prePostCode } example
  putStrLn $ reportSchedule schedule

prePostCode :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)
prePostCode _ _ _ =
  ( unlines
    [ "#include <stdlib.h>"
    , "#include <stdio.h>"
    , "unsigned long int a;"
    , "unsigned long int b;"
    , "unsigned long int x;"
    , "unsigned char running = 1;"
    ]
  , unlines
    [ "int main(int argc, char* argv[]) {"
    , "  if (argc < 3) {"
    , "    printf(\"usage: gcd <num1> <num2>\\n\");"
    , "  }"
    , "  else {"
    , "    a = atoi(argv[1]);"
    , "    b = atoi(argv[2]);"
    , "    printf(\"Computing the GCD of %lu and %lu...\\n\", a, b);"
    , "    while(running) {"
    , "      example();"
    , "      printf(\"iteration:  a = %lu  b = %lu\\n\", a, b);"
    , "    }"
    , "    printf(\"GCD result: %lu\\n\", a);"
    , "  }"
    , "  return 0;"
    , "}"
    ]
  )

-- | An example design that computes the greatest common divisor.
example :: Atom ()
example = do

  -- External reference to value A.
  let a = word32' "a"

  -- External reference to value B.
  let b = word32' "b"

  -- The external running flag.
  let running = bool' "running"

  -- A rule to modify A.
  atom "a_minus_b" $ do
    cond $ value a >. value b
    a <== value a - value b

  -- A rule to modify B.
  atom "b_minus_a" $ do
    cond $ value b >. value a
    b <== value b - value a

  -- A rule to clear the running flag.
  atom "stop" $ do
    cond $ value a ==. value b
    running <== false

