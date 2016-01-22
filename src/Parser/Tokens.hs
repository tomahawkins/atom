module Parser.Tokens
  ( Token     (..)
  , TokenName (..)
  , tokenString
  ) where

import Common

tokenString :: Token -> String
tokenString (Token _ s _) = s

data Token = Token TokenName String Location deriving (Show, Eq)

instance Locate Token    where locate (Token _ _ l) = l

data TokenName
  = InfixL9 | InfixR9 | Infix9
  | InfixL8 | InfixR8 | Infix8
  | InfixL7 | InfixR7 | Infix7
  | InfixL6 | InfixR6 | Infix6
  | InfixL5 | InfixR5 | Infix5
  | InfixL4 | InfixR4 | Infix4
  | InfixL3 | InfixR3 | Infix3
  | InfixL2 | InfixR2 | Infix2
  | InfixL1 | InfixR1 | Infix1
  | InfixL0 | InfixR0 | Infix0

  | ParenL | ParenR
  | Equal
  | ColonColon
  | Semi
  | Tic
  | Pipe
  | Unit

  | KW_datatype
  | KW_class
  | KW_instance
  | KW_case
  | KW_if
  | KW_else
  | KW_do
  | KW_of
  | KW_where
  | KW_intrinsic

  | IdUpper
  | IdLower
  | Operator

  | Unknown
  deriving (Show, Eq)

