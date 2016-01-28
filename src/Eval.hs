module Eval (eval) where

import AST
import Parser

eval :: [Module [TopDeclaration]] -> IO ()
eval a = mapM_ print $ concat [ d | Module _ _ _ _ _ d <- a ]

