{
module Parser.Parse (topDeclarations) where

import AST
import Parser.Tokens
}

%name      topDeclarations
%tokentype { Token }
%error     { parseError }

%expect 0

%token

"case"      { Token KW_case     _ _ }
"if"        { Token KW_if       _ _ }
"else"      { Token KW_else     _ _ }
"do"        { Token KW_do       _ _ }
"of"        { Token KW_of       _ _ }
"where"     { Token KW_where    _ _ }

"infixl9" { Token InfixL9 _ _ }    "infixr9" { Token InfixR9 _ _ }    "infix9" { Token Infix9 _ _ }
"infixl8" { Token InfixL8 _ _ }    "infixr8" { Token InfixR8 _ _ }    "infix8" { Token Infix8 _ _ }
"infixl7" { Token InfixL7 _ _ }    "infixr7" { Token InfixR7 _ _ }    "infix7" { Token Infix7 _ _ }
"infixl6" { Token InfixL6 _ _ }    "infixr6" { Token InfixR6 _ _ }    "infix6" { Token Infix6 _ _ }
"infixl5" { Token InfixL5 _ _ }    "infixr5" { Token InfixR5 _ _ }    "infix5" { Token Infix5 _ _ }
"infixl4" { Token InfixL4 _ _ }    "infixr4" { Token InfixR4 _ _ }    "infix4" { Token Infix4 _ _ }
"infixl3" { Token InfixL3 _ _ }    "infixr3" { Token InfixR3 _ _ }    "infix3" { Token Infix3 _ _ }
"infixl2" { Token InfixL2 _ _ }    "infixr2" { Token InfixR2 _ _ }    "infix2" { Token Infix2 _ _ }
"infixl1" { Token InfixL1 _ _ }    "infixr1" { Token InfixR1 _ _ }    "infix1" { Token Infix1 _ _ }
"infixl0" { Token InfixL0 _ _ }    "infixr0" { Token InfixR0 _ _ }    "infix0" { Token Infix0 _ _ }

"()"         { Token Unit        _ _ }
"("          { Token ParenL      _ _ }
")"          { Token ParenR      _ _ }

identifier   { Token Id          _ _ }

%%

TopDeclarations :: { [TopDeclaration] }
: { [] }
| TopDeclarations TopDeclaration  { $1 ++ [$2] }

TopDeclaration :: { TopDeclaration }
: "()" { Value }

Identifier :: { String }
: identifier   { tokenString $1 }

Identifiers :: { [String] }
:             Identifier { [$1] }
| Identifiers Identifier { $1 ++ [$2] }

Expression :: { () }
: Expr9 { $1 }

Expr9 :: { () } : Expr9 "infixl9" Expr8 { () } | Expr8 "infixr9" Expr9 { () } | Expr8 "infix9"  Expr8 { () }
Expr8 :: { () } : Expr8 "infixl8" Expr7 { () } | Expr7 "infixr8" Expr8 { () } | Expr7 "infix8"  Expr7 { () }
Expr7 :: { () } : Expr7 "infixl7" Expr6 { () } | Expr6 "infixr7" Expr7 { () } | Expr6 "infix7"  Expr6 { () }
Expr6 :: { () } : Expr6 "infixl6" Expr5 { () } | Expr5 "infixr6" Expr6 { () } | Expr5 "infix6"  Expr5 { () }
Expr5 :: { () } : Expr5 "infixl5" Expr4 { () } | Expr4 "infixr5" Expr5 { () } | Expr4 "infix5"  Expr4 { () }
Expr4 :: { () } : Expr4 "infixl4" Expr3 { () } | Expr3 "infixr4" Expr4 { () } | Expr3 "infix4"  Expr3 { () }
Expr3 :: { () } : Expr3 "infixl3" Expr2 { () } | Expr2 "infixr3" Expr3 { () } | Expr2 "infix3"  Expr2 { () }
Expr2 :: { () } : Expr2 "infixl2" Expr1 { () } | Expr1 "infixr2" Expr2 { () } | Expr1 "infix2"  Expr1 { () }
Expr1 :: { () } : Expr1 "infixl1" Expr0 { () } | Expr0 "infixr1" Expr1 { () } | Expr0 "infix1"  Expr0 { () }
Expr0 :: { () } : Expr0 "infixl0" ExprP { () } | ExprP "infixr0" Expr0 { () } | ExprP "infix0"  ExprP { () }

ExprP :: { () }
: Identifier         { () }
| "(" Expression ")" { $2 }
| ExprP ExprP        { () }





{
parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

toString :: Token -> String
toString = tail . init . tokenString
}

