module Eval (eval) where

import AST
import Parser

eval :: [Module [TopDeclaration]] -> IO ()
eval = const $ return ()

