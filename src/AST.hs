module AST
  ( TopDeclaration (..)
  , Value (..)
  , Expr (..)
  ) where

import Common

data TopDeclaration
  = Value'    Value
  | Datatype  Location Name [Name] [(Location, Name, [Expr])]
  | Typeclass
  deriving Show

data Value = Value
  { loc       :: Location
  , name      :: Name
  , contract  :: Maybe Expr
  , arguments :: [Name]
  , body      :: Expr
  } deriving Show

instance Locate Value where locate = loc

type L = Location

data Expr
  = LitUnit   L
  | VarValue  L Name
  | VarType   L Name
  | Apply     L Expr Expr
  | Lambda    L Name Expr
  | Where     L Expr [Value]
  | Intrinsic L Name
  deriving Show

instance Locate Expr     where
  locate a = case a of
    LitUnit   a -> a
    VarValue  a _ -> a
    VarType   a _ -> a
    Apply     a _ _ -> a
    Lambda    a _ _ -> a
    Where     a _ _ -> a
    Intrinsic a _ -> a
    

