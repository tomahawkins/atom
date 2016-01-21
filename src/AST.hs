module AST
  ( TopDeclaration (..)
  , ValueDeclaration (..)
  , Parameter (..)
  , Expr (..)
  , Literal (..)
  , Locate (..)
  ) where

import Common
import Parser.Tokens

data TopDeclaration
  = Datatype  Name [Name] [(Name, [Parameter])]
  | Typeclass
  | Value     ValueDeclaration
  deriving Show

data Parameter
  = Type     Name
  | Abstract Name
  deriving Show

data ValueDeclaration = ValueDeclaration
  { name      :: Name
  , arguments :: [(Name, Maybe Expr)]
  , contract  :: Maybe Expr
  , body      :: Expr
  } deriving Show

type L = Location

data Expr
  = Literal  L Literal
  | VarValue L Name
  | VarType  L Name
  | Apply    L Expr Expr
  deriving Show

data Literal
  = LitUnit
  deriving Show

class    Locate a        where locate :: a -> Location
instance Locate Location where locate = id
instance Locate Token    where locate (Token _ _ l) = l
instance Locate Expr     where
  locate a = case a of
    Literal   a _ -> a
    VarValue  a _ -> a
    VarType   a _ -> a
    Apply     a _ _ -> a
    

{-
data TopLevelDeclaration
  = TypeClass Name Name [
  | NamedValue Name [(Name, CtrExpr)] (Maybe CtrExpr) Expr
data Prototype
-}

