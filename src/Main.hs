module Main (main) where

import System.Environment

import Parser
import Rules  ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> parseProgram file
    _ -> putStrLn "usage: atom program.atom"

