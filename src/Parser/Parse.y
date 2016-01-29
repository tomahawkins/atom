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

idLower   { Token IdLower     _ _ }
idUpper   { Token IdUpper     _ _ }

"case"       { Token KW_case       _ _ }
"class"      { Token KW_class      _ _ }
"datatype"   { Token KW_datatype   _ _ }
"do"         { Token KW_do         _ _ }
"else"       { Token KW_else       _ _ }
"if"         { Token KW_if         _ _ }
"instance"   { Token KW_instance   _ _ }
"intrinsic"  { Token KW_intrinsic  _ _ }
"let"        { Token KW_let        _ _ }
"of"         { Token KW_of         _ _ }
"then"       { Token KW_then       _ _ }
"where"      { Token KW_where      _ _ }

"()"  { Token Unit          _ _ }
"("   { Token ParenL        _ _ }
")"   { Token ParenR        _ _ }
"::"  { Token ColonColon    _ _ }
"="   { Token Equal         _ _ }
";"   { Token Semi          _ _ }
--"`"   { Token Tic           _ _ }
"|"   { Token Pipe          _ _ }
"\\"  { Token Backslash     _ _ }
"_"   { Token Underscore    _ _ }
"@"   { Token At            _ _ }

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

%%

TopDeclarations :: { [TopDeclaration] }
: { [] }
| TopDeclarations TopDeclaration  { $1 ++ [$2] }

TopDeclaration :: { TopDeclaration }
: Value                                            { Value' $1 }
| "datatype" IdUpper IdLowers "=" Constructors ";" { Datatype (locate $1) (snd $2) $3 $5 }

Value :: { Value }
: ValueId                               "=" Expression ";" { Value (fst $1) (snd $1) Nothing   [] $3 }
| ValueId "::" Expression               "=" Expression ";" { Value (fst $1) (snd $1) (Just $3) [] $5 }
| ValueId                     IdLowers_ "=" Expression ";" { Value (fst $1) (snd $1) Nothing   $2 $4 }
| ValueId "::" Expression "|" IdLowers_ "=" Expression ";" { Value (fst $1) (snd $1) (Just $3) $5 $7 }

Values :: { [Value] }
:              { [] }
| Values Value { $1 ++ [$2] }

ValueId :: { (Location, Name) }
: IdLower            { $1 }
| "(" Operator ")"   { $2 }

IdLowers_ :: { [Name] }
: IdLowers IdLower { $1 ++ [snd $2] }

IdLowers :: { [Name] }
:                   { [] }
| IdLowers IdLower  { $1 ++ [snd $2] }

Constructors  :: { [(Location, Name, [Expr])] }
:                  IdLower Expressions  {       [(fst $1, snd $1, $2)] }
| Constructors "|" IdLower Expressions  { $1 ++ [(fst $3, snd $3, $4)] }

IdLower :: { (Location, Name) }
: idLower  { tokenLocStr $1 }

IdUpper :: { (Location, Name) }
: idUpper  { tokenLocStr $1 }

Operator :: { (Location, Name) }
: "infix0" { tokenLocStr $1 } | "infixl0" { tokenLocStr $1 } | "infixr0" { tokenLocStr $1 }
| "infix1" { tokenLocStr $1 } | "infixl1" { tokenLocStr $1 } | "infixr1" { tokenLocStr $1 }
| "infix2" { tokenLocStr $1 } | "infixl2" { tokenLocStr $1 } | "infixr2" { tokenLocStr $1 }
| "infix3" { tokenLocStr $1 } | "infixl3" { tokenLocStr $1 } | "infixr3" { tokenLocStr $1 }
| "infix4" { tokenLocStr $1 } | "infixl4" { tokenLocStr $1 } | "infixr4" { tokenLocStr $1 }
| "infix5" { tokenLocStr $1 } | "infixl5" { tokenLocStr $1 } | "infixr5" { tokenLocStr $1 }
| "infix6" { tokenLocStr $1 } | "infixl6" { tokenLocStr $1 } | "infixr6" { tokenLocStr $1 }
| "infix7" { tokenLocStr $1 } | "infixl7" { tokenLocStr $1 } | "infixr7" { tokenLocStr $1 }
| "infix8" { tokenLocStr $1 } | "infixl8" { tokenLocStr $1 } | "infixr8" { tokenLocStr $1 }
| "infix9" { tokenLocStr $1 } | "infixl9" { tokenLocStr $1 } | "infixr9" { tokenLocStr $1 }
--| "`" IdLower "`" { $2 }

Cases :: { [(Pattern, Guard)] }
:       Case  { [$1] }
| Cases Case  { $1 ++ [$2] }

Case :: { (Pattern, Guard) }
: Pattern Guard  ";" { ($1, $2) }

Guard :: { Guard }
:                "=" Expression        { Unguarded (locate $2) $2 }
| "|" Expression "=" Expression        { Guard     (locate $2) $2 $4 }
| "|" Expression "=" Expression Guard  { Guard'    (locate $2) $2 $4 $5 }

Pattern :: { Pattern }
: Pattern0 { $1 }

Pattern0 :: { Pattern }
: Pattern0 Pattern1 { PatternApply (locate $1) $1 $2 }
|          Pattern1 { $1 }

Pattern1 :: { Pattern }
: IdLower "@" Pattern1  { As (locate $2) (snd $1) $3 }
| Pattern2              { $1 }

Pattern2 :: { Pattern }
: "_"                { Wildcard $ locate $1 }
| IdLower            { PatternVar (fst $1) (snd $1) }
| "(" Pattern ")"    { $2 }

DoItem :: { () }
: "let" Value                 { () }
| IdLower "=" Expression ";"  { () }
| Expression ";"              { () }

DoItems :: { [()] }
: DoItem { [$1] }
| DoItems DoItem { $1 ++ [$2] }

Expressions :: { [Expr] }
: { [] }
| Expressions ExprPrimary { $1 ++ [$2] }

Expression :: { Expr }
: Expr0 { $1 }

Expr0 :: { Expr }
: "\\" IdLowers_ "=" Expr0             { lambda (locate $1) $2 $4 }
| "if" Expr0 "then" Expr0 "else" Expr0 { If (locate $1) $2 $4 $6 }
| "case" Expr0 "of" Cases              { Case (locate $1) $2 $4 }
| Expr1 "::" Expr0                     { ApplyContract (locate $2) $1 $3 }
| "do" DoItems                         { undefined }
| Expr1                                { $1 }

Expr1 :: { Expr }
: Expr1 "where" Values  { Where (locate $1) $1 $3 }
| Expr0a { $1 }

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
: "(" Expression ")"             { $2 }
| IdLower                        { VarValue (fst $1) (snd $1) }
| IdUpper                        { VarType  (fst $1) (snd $1) }
| "(" Operator ")"               { VarValue (fst $2) (snd $2) }
--| "(" Expression Operator ")"    { Apply    (fst $3) (VarValue (fst $3) (snd $3)) $2 }
--| "(" Operator Expression ")"    { () }
| "()"                           { LitUnit $ locate $1 }
| "intrinsic" IdLower            { Intrinsic (locate $1) (snd $2) }



{
parseError :: [Token] -> a
parseError a = case a of
  []              -> error "Parse error: no tokens left to parse."
  Token t s p : _ -> error $ "Parse error: unexpected token '" ++ s ++ "' (" ++ show t ++ ") at " ++ show p ++ "."

applyInfix :: Expr -> Token -> Expr -> Expr
applyInfix a op b = Apply (locate op) (Apply (locate op) (VarValue (locate op) (tokenString op)) a) b

tokenLocStr :: Token -> (Location, String)
tokenLocStr a = (locate a, tokenString a)

lambda :: Location -> [Name] -> Expr -> Expr
lambda l a b = case a of
  [] -> error "Error on lambda expression."
  [a] -> Lambda l a b
  a : as -> Lambda l a $ lambda l as b
}

