module Parser.Tokens
  ( Token     (..)
  , TokenName (..)
  , Position  (..)
  , tokenString
  ) where

tokenString :: Token -> String
tokenString (Token _ s _) = s

data Position = Position String Int Int deriving Eq

instance Show Position where
  show (Position f l c) = f ++ ":" ++ show l ++ ":" ++ show c

data Token = Token TokenName String Position deriving (Show, Eq)

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
  | Unit

  | KW_case
  | KW_if
  | KW_else
  | KW_do
  | KW_of
  | KW_where

  | Id
  | Unknown
  deriving (Show, Eq)

