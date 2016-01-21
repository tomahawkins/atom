{
module Parser.Parse (topDeclarations) where

import AST
import Common
import Parser.Tokens

}

%name      topDeclarations
%tokentype { Token }
%error     { parseError }

%expect 0

%token

"class"     { Token KW_class    _ _ }
"instance"  { Token KW_instance _ _ }
"datatype"  { Token KW_datatype _ _ }
"value"     { Token KW_value    _ _ }
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

"()"  { Token Unit        _ _ }
"("   { Token ParenL      _ _ }
")"   { Token ParenR      _ _ }
"::"  { Token ColonColon  _ _ }
"="   { Token Equal       _ _ }
";"   { Token Semi        _ _ }
"`"   { Token Tic         _ _ }
"|"   { Token Pipe        _ _ }

idLower   { Token IdLower     _ _ }
idUpper   { Token IdUpper     _ _ }
operator  { Token Operator    _ _ }

%%

TopDeclarations :: { [TopDeclaration] }
: { [] }
| TopDeclarations TopDeclaration  { $1 ++ [$2] }

TopDeclaration :: { TopDeclaration }
: ValueDeclaration                               { Value $1 }
--| "datatype" IdUpper Parameters "=" Constructors { Value }

ValueDeclaration :: { ValueDeclaration }
: "value" ValueId Arguments                          "=" Expression  { ValueDeclaration $2 $3 Nothing $5       }
| "value" ValueId Arguments          "::" Expression "=" Expression  { ValueDeclaration $2 $3 (Just $5) $7            }
| "value" Argument Operator Argument                 "=" Expression  { ValueDeclaration (snd $3) [$2, $4] Nothing $6 }
| "value" Argument Operator Argument "::" Expression "=" Expression  { ValueDeclaration (snd $3) [$2, $4] (Just $6) $8      }

ValueId :: { Name }
: IdLower            { snd $1 }
| "(" Operator ")"   { snd $2 }

Arguments :: { [(Name, Maybe Expr)] }
:                     { [] }
| Arguments Argument  { $1 ++ [$2] }

Argument :: { (Name, Maybe Expr) }
:     IdLower                        { (snd $1, Nothing) }
| "(" IdLower "::" Expression ")"    { (snd $2, Just $4) }

Parameters :: { [Name] }
:                     { [] }
| Parameters IdLower  { $1 ++ [snd $2] }

Constructors  :: { () }
:                  IdLower Expressions  { () }
| Constructors "|" IdUpper Expressions  { () }

IdLower :: { (Location, Name) }
: idLower  { tokenLocStr $1 }

IdUpper :: { (Location, Name) }
: idUpper  { tokenLocStr $1 }

Operator:: { (Location, Name) }
: operator        { tokenLocStr $1 }
| "`" IdLower "`" { $2 }

Expressions :: { [Expr] }
: { [] }
| Expressions ExprPrimary { $1 ++ [$2] }

Expression :: { Expr }
: Expr0a { $1 }

Expr0a :: { Expr } : Expr1a "infix0" Expr1a { applyInfix $1 $2 $3 } | Expr0a "infixl0" Expr0b { applyInfix $1 $2 $3 } | Expr0b { $1 }    Expr0b :: { Expr } : Expr1a "infixr0" Expr0b { applyInfix $1 $2 $3 } | Expr1a  { $1 }
Expr1a :: { Expr } : Expr2a "infix1" Expr2a { applyInfix $1 $2 $3 } | Expr1a "infixl1" Expr1b { applyInfix $1 $2 $3 } | Expr1b { $1 }    Expr1b :: { Expr } : Expr2a "infixr1" Expr1b { applyInfix $1 $2 $3 } | Expr2a  { $1 }
Expr2a :: { Expr } : Expr3a "infix2" Expr3a { applyInfix $1 $2 $3 } | Expr2a "infixl2" Expr2b { applyInfix $1 $2 $3 } | Expr2b { $1 }    Expr2b :: { Expr } : Expr3a "infixr2" Expr2b { applyInfix $1 $2 $3 } | Expr3a  { $1 }
Expr3a :: { Expr } : Expr4a "infix3" Expr4a { applyInfix $1 $2 $3 } | Expr3a "infixl3" Expr3b { applyInfix $1 $2 $3 } | Expr3b { $1 }    Expr3b :: { Expr } : Expr4a "infixr3" Expr3b { applyInfix $1 $2 $3 } | Expr4a  { $1 }
Expr4a :: { Expr } : Expr5a "infix4" Expr5a { applyInfix $1 $2 $3 } | Expr4a "infixl4" Expr4b { applyInfix $1 $2 $3 } | Expr4b { $1 }    Expr4b :: { Expr } : Expr5a "infixr4" Expr4b { applyInfix $1 $2 $3 } | Expr5a  { $1 }
Expr5a :: { Expr } : Expr6a "infix5" Expr6a { applyInfix $1 $2 $3 } | Expr5a "infixl5" Expr5b { applyInfix $1 $2 $3 } | Expr5b { $1 }    Expr5b :: { Expr } : Expr6a "infixr5" Expr5b { applyInfix $1 $2 $3 } | Expr6a  { $1 }
Expr6a :: { Expr } : Expr7a "infix6" Expr7a { applyInfix $1 $2 $3 } | Expr6a "infixl6" Expr6b { applyInfix $1 $2 $3 } | Expr6b { $1 }    Expr6b :: { Expr } : Expr7a "infixr6" Expr6b { applyInfix $1 $2 $3 } | Expr7a  { $1 }
Expr7a :: { Expr } : Expr8a "infix7" Expr8a { applyInfix $1 $2 $3 } | Expr7a "infixl7" Expr7b { applyInfix $1 $2 $3 } | Expr7b { $1 }    Expr7b :: { Expr } : Expr8a "infixr7" Expr7b { applyInfix $1 $2 $3 } | Expr8a  { $1 }
Expr8a :: { Expr } : Expr9a "infix8" Expr9a { applyInfix $1 $2 $3 } | Expr8a "infixl8" Expr8b { applyInfix $1 $2 $3 } | Expr8b { $1 }    Expr8b :: { Expr } : Expr9a "infixr8" Expr8b { applyInfix $1 $2 $3 } | Expr9a  { $1 }
Expr9a :: { Expr } : ExprAp "infix9" ExprAp { applyInfix $1 $2 $3 } | Expr9a "infixl9" Expr9b { applyInfix $1 $2 $3 } | Expr9b { $1 }    Expr9b :: { Expr } : ExprAp "infixr9" Expr9b { applyInfix $1 $2 $3 } | ExprAp  { $1 }

ExprAp :: { Expr }
: ExprAp ExprPrimary { Apply (locate $1) $1 $2 }
|        ExprPrimary { $1 }

ExprPrimary :: { Expr }
: IdLower                       { VarValue (fst $1) (snd $1) }
| IdUpper                       { VarType  (fst $1) (snd $1) }
| "(" Operator ")"              { VarValue (fst $2) (snd $2) }
--| "(" Operator Expression ")"   { () }
--| "(" Expression Operator ")"   { () }
| "(" Expression ")"            { $2 }
| ExprLiteral                   { $1 }

ExprLiteral :: { Expr }
: "()"     { Literal (locate $1) LitUnit }



{
parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

applyInfix :: Expr -> Token -> Expr -> Expr
applyInfix a op b = Apply (locate op) (Apply (locate op) (VarValue (locate op) (tokenString op)) a) b
}

