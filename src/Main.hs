module Main (main) where

import System.Environment

import Eval
import Parser
import Rules  ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> parseProgram file >>= eval
    _ -> putStrLn "usage: atom program.atom"

