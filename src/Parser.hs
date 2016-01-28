module Parser
  ( Module (..)
  , parseProgram
  ) where

import Control.Monad
import Data.List
import Data.Maybe

import AST
import Common
import Parser.Lex
import Parser.Parse
import Parser.Tokens

data Module a = Module FilePath Path [Import] [Export] [InfixDef] a deriving Show
data Import = Import Path deriving Show
data Export = Export Name deriving Show
type InfixDef = (String, (Int, Associativity))
data Associativity = AssocLeft | AssocRight | AssocNone deriving Show

-- Parse a program.
parseProgram :: FilePath -> IO [Module [TopDeclaration]]
parseProgram main
  | isSuffixOf ".atom" main = do
    modules <- parseModules [] $ split '/' $ take (length main - 5) main
    return $ map (parseCode $ infixDefs modules) modules
  | otherwise = error "Expecting an *.atom file."

-- Parse a single module.
parseModule :: FilePath -> Path -> String -> Module String
parseModule file path a = Module file path imports' exports' infixes' a4
  where
  a1 = uncomment file a
  (a2, imports') = imports a1
  (a3, exports') = exports a2
  (a4, infixes') = infixes a3

-- Parse all the modules of a program.
parseModules :: [Module String] -> Path -> IO [Module String]
parseModules sofar path
  | isJust $ find (\ (Module _ p _ _ _ _) -> p == path) sofar = return sofar
  | otherwise = do
    f <- readFile file
    let m@(Module _ _ imports _ _ _) = parseModule file path f
    foldM parseModules (sofar ++ [m]) [ i | Import i <- imports ]
    where
    file = intercalate "/" path ++ ".atom"

-- Given a list of program modules, build a list of infix definitions (precedence and associativity).
infixDefs :: [Module a] -> [InfixDef]
infixDefs modules
  | null duplicates = defs
  | otherwise       = error $ "Duplicate infix symbols: " ++ show duplicates
  where
  defs = concat [ a | Module _ _ _ _ a _ <- modules ]
  symbols = fst $ unzip defs
  duplicates = symbols \\ nub symbols

-- Looks up a symbol in the infix table to change the token.
infixName :: (Int, Associativity) -> TokenName
infixName (prec, assoc) = case prec of
  0 -> case assoc of { AssocLeft -> InfixL0; AssocRight -> InfixR0; AssocNone -> Infix0 }
  1 -> case assoc of { AssocLeft -> InfixL1; AssocRight -> InfixR1; AssocNone -> Infix1 }
  2 -> case assoc of { AssocLeft -> InfixL2; AssocRight -> InfixR2; AssocNone -> Infix2 }
  3 -> case assoc of { AssocLeft -> InfixL3; AssocRight -> InfixR3; AssocNone -> Infix3 }
  4 -> case assoc of { AssocLeft -> InfixL4; AssocRight -> InfixR4; AssocNone -> Infix4 }
  5 -> case assoc of { AssocLeft -> InfixL5; AssocRight -> InfixR5; AssocNone -> Infix5 }
  6 -> case assoc of { AssocLeft -> InfixL6; AssocRight -> InfixR6; AssocNone -> Infix6 }
  7 -> case assoc of { AssocLeft -> InfixL7; AssocRight -> InfixR7; AssocNone -> Infix7 }
  8 -> case assoc of { AssocLeft -> InfixL8; AssocRight -> InfixR8; AssocNone -> Infix8 }
  9 -> case assoc of { AssocLeft -> InfixL9; AssocRight -> InfixR9; AssocNone -> Infix9 }
  _ -> error $ "Invalid infix precedence level: " ++ show prec

-- Parses a module's code.
parseCode :: [InfixDef] -> Module String -> Module [TopDeclaration]
parseCode infixDefs (Module file a b c d content) = Module file a b c d $ topDeclarations tokens
  where
  tokens = map (changeInfix . relocate) $ alexScanTokens content
  relocate :: Token -> Token
  relocate (Token t s (Location _ l c)) = Token t s $ Location file l c
  changeInfix :: Token -> Token
  changeInfix (Token t s p) = case t of
    InfixL9 -> case lookup s infixDefs of
      Nothing -> Token t s p
      Just a  -> Token (infixName a) s p
    _ -> Token t s p

-- | Remove comments from code.
uncomment :: FilePath -> String -> String
uncomment file a = uncomment a
  where
  uncomment a = case a of
    ""               -> ""
    '-' : '-' : rest -> "  " ++ removeEOL rest
    '{' : '-' : rest -> "  " ++ remove 1 rest
    '"'       : rest -> '"' : ignoreString rest
    a         : rest -> a   : uncomment rest

  removeEOL a = case a of
    ""          -> ""
    '\n' : rest -> '\n' : uncomment rest 
    '\t' : rest -> '\t' : removeEOL rest
    _    : rest -> ' '  : removeEOL rest

  remove depth a = case a of
    ""               -> error $ "File ended without closing comment (-}): " ++ file
    '"' : rest       -> removeString depth rest
    '\n' : rest      -> '\n' : remove depth rest
    '\t' : rest      -> '\t' : remove depth rest
    '-' : '}' : rest -> "  " ++ (if depth == 1 then uncomment else remove $ depth - 1) rest
    '{' : '-' : rest -> "  " ++ remove (depth + 1) rest
    _ : rest         -> " "  ++ remove depth rest

  removeString depth a = case a of
    ""                -> error $ "File ended without closing string: " ++ file
    '"' : rest        -> " "  ++ remove       depth rest
    '\\' : '"' : rest -> "  " ++ removeString depth rest
    '\n' : rest       -> '\n' :  removeString depth rest
    '\t' : rest       -> '\t' :  removeString depth rest
    _    : rest       -> ' '  :  removeString depth rest

  ignoreString a = case a of
    ""                -> error $ "File ended without closing string: " ++ file
    '"' : rest        -> '"' : uncomment rest
    '\\' : '"' : rest -> "\\\"" ++ ignoreString rest
    a : rest          -> a : ignoreString rest

-- Extracts imports from a module.
imports :: String -> (String, [Import])
imports = filterExtract $ \ a -> case words a of
  ["import", p] -> Just $ Import $ split '.' p
  _ -> Nothing

-- Extracts exports from a module.
exports :: String -> (String, [Export])
exports = filterExtract $ \ a -> case words a of
  ["export", p] -> Just $ Export p
  _ -> Nothing

-- Extracts the infix declarations from a module.
infixes :: String -> (String, [InfixDef])
infixes = filterExtract $ \ a -> case words a of
  ["infix",  n, a] | elem n $ map show [0 .. 9] -> Just (a, (read n, AssocNone ))
  ["infixl", n, a] | elem n $ map show [0 .. 9] -> Just (a, (read n, AssocLeft ))
  ["infixr", n, a] | elem n $ map show [0 .. 9] -> Just (a, (read n, AssocRight))
  _ -> Nothing

-- Filters out lines and accumulates data on a pattern.
filterExtract :: (String -> Maybe a) -> String -> (String, [a])
filterExtract f a = (unlines m, catMaybes n)
  where
  (m, n) = unzip . map k . lines $ a
  k a = case f a of
    Nothing -> (a, Nothing)
    Just b  -> ("", Just b)

