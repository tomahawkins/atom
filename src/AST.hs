module AST
  ( TopDeclaration (..)
  ) where

import Common

data TopDeclaration
  = Datatype  Name [Name] [(Name, [Parameter])]
  | Typeclass
  | Value
  deriving Show

data Parameter
  = Type     Name
  | Abstract Name
  deriving Show

{-
data TopLevelDeclaration
  = TypeClass Name Name [
  | NamedValue Name [(Name, CtrExpr)] (Maybe CtrExpr) Expr
data Prototype
-}

