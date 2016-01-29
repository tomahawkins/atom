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
  | Backslash
  | Underscore
  | At
  | Unit

  | KW_case
  | KW_class
  | KW_datatype
  | KW_do
  | KW_else
  | KW_if
  | KW_instance
  | KW_intrinsic
  | KW_let
  | KW_of
  | KW_then
  | KW_where

  | IdUpper
  | IdLower

  | Unknown
  deriving (Show, Eq)

