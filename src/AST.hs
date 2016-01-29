module AST
  ( TopDeclaration (..)
  , Value   (..)
  , Expr    (..)
  , Guard   (..)
  , Pattern (..)
  ) where

import Common

data TopDeclaration
  = Value'    Value
  | Datatype  Location Name [Name] [(Location, Name, [Expr])]
  | Typeclass Location
  deriving Show

instance Locate TopDeclaration where
  locate a = case a of
    Value' a -> locate a
    Datatype a _ _ _ -> a
    Typeclass a -> a

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
  = LitUnit       L
  | VarValue      L Name
  | VarType       L Name
  | Apply         L Expr Expr
  | ApplyContract L Expr Expr
  | Lambda        L Name Expr
  | Where         L Expr [Value]
  | Intrinsic     L Name
  | If            L Expr Expr Expr
  | Case          L Expr [(Pattern, Guard)]
  deriving Show

instance Locate Expr     where
  locate a = case a of
    LitUnit       a -> a
    VarValue      a _ -> a
    VarType       a _ -> a
    Apply         a _ _ -> a
    ApplyContract a _ _ -> a
    Lambda        a _ _ -> a
    Where         a _ _ -> a
    Intrinsic     a _ -> a
    If            a _ _ _ -> a
    Case          a _ _ -> a
    
data Pattern
  = Wildcard     L
  | PatternVar   L Name
  | PatternApply L Pattern Pattern
  | As           L Name Pattern
  deriving Show

instance Locate Pattern where
  locate a = case a of
    Wildcard     a -> a
    PatternVar   a _ -> a
    PatternApply a _ _ -> a
    As           a _ _ -> a

data Guard
  = Unguarded L Expr
  | Guard     L Expr Expr
  | Guard'    L Expr Expr Guard
  deriving Show

instance Locate Guard where
  locate a = case a of
    Unguarded a _ -> a
    Guard     a _ _ -> a
    Guard'    a _ _ _ -> a

