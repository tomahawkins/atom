module Main (main) where

import System.Environment

import Eval
import Parser
import Rules  ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      m <- parseProgram file
      mapM_ print m
      eval m
    _ -> putStrLn "usage: atom program.atom"

