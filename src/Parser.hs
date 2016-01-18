module Parser (parseProgram) where

import Control.Monad
import Data.List
import Data.Maybe

import AST
import Common
import Parser.Lex
import Parser.Parse
import Parser.Tokens

data Module = Module FilePath Path [Import] [Export] [Infix] String

-- Parse a single module.
parseModule :: FilePath -> Path -> String -> Module
parseModule file path a = Module file path imports' exports' infixes' a4
  where
  a1 = uncomment file a
  (a2, imports') = imports a1
  (a3, exports') = exports a2
  (a4, infixes') = infixes a3

-- Parse all the modules of a program.
parseModules :: [Module] -> Path -> IO [Module]
parseModules sofar path
  | isJust $ find (\ (Module _ p _ _ _ _) -> p == path) sofar = return sofar
  | otherwise = do
    putStrLn file
    f <- readFile file
    let m@(Module _ _ imports _ _ _) = parseModule file path f
    foldM parseModules (sofar ++ [m]) [ i | Import i <- imports ]
    where
    file = intercalate "/" path ++ ".atom"

-- Parse a program.
parseProgram :: FilePath -> IO ()
parseProgram main
  | isSuffixOf ".atom" main = do
    modules <- parseModules [] [reverse $ drop 5 $ reverse main]
    m <- mapM parseCode modules
    print m
  | otherwise = error "Expecting *.atom file."

parseCode :: Module -> IO [TopDeclaration]
parseCode (Module file _ _ _ _ content) = do
  putStrLn $ "Parsing " ++ show file ++ " ..."
  return $ topDeclarations tokens
  where
  tokens = map relocate $ alexScanTokens content
  relocate :: Token -> Token
  relocate (Token t s (Position _ l c)) = Token t s $ Position file l c

-- | Remove comments from code.
uncomment :: FilePath -> String -> String
uncomment file a = uncomment a
  where
  uncomment a = case a of
    ""               -> ""
    '-' : '-' : rest -> "  " ++ removeEOL rest
    '{' : '-' : rest -> "  " ++ remove rest
    '"'       : rest -> '"' : ignoreString rest
    a         : rest -> a   : uncomment rest

  removeEOL a = case a of
    ""          -> ""
    '\n' : rest -> '\n' : uncomment rest 
    '\t' : rest -> '\t' : removeEOL rest
    _    : rest -> ' '  : removeEOL rest

  remove a = case a of
    ""               -> error $ "File ended without closing comment (-}): " ++ file
    '"' : rest       -> removeString rest
    '\n' : rest      -> '\n' : remove rest
    '\t' : rest      -> '\t' : remove rest
    '-' : '}' : rest -> "  " ++ uncomment rest
    _ : rest         -> " "  ++ remove rest

  removeString a = case a of
    ""                -> error $ "File ended without closing string: " ++ file
    '"' : rest        -> " "  ++ remove       rest
    '\\' : '"' : rest -> "  " ++ removeString rest
    '\n' : rest       -> '\n' :  removeString rest
    '\t' : rest       -> '\t' :  removeString rest
    _    : rest       -> ' '  :  removeString rest

  ignoreString a = case a of
    ""                -> error $ "File ended without closing string: " ++ file
    '"' : rest        -> '"' : uncomment rest
    '\\' : '"' : rest -> "\\\"" ++ ignoreString rest
    a : rest          -> a : ignoreString rest

data Import = Import Path
data Export = Export Name

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

data Infix = Infix Int String | Infixl Int String | Infixr Int String

-- Extracts the infix declarations from a module.
infixes :: String -> (String, [Infix])
infixes = filterExtract $ \ a -> case words a of
  ["infix",  n, a] | elem n $ map show [0 .. 9] -> Just $ Infix  (read n) a
  ["infixl", n, a] | elem n $ map show [0 .. 9] -> Just $ Infixl (read n) a
  ["infixr", n, a] | elem n $ map show [0 .. 9] -> Just $ Infixr (read n) a
  _ -> Nothing

-- Filters out lines and accumulates data on pattern.
filterExtract :: (String -> Maybe a) -> String -> (String, [a])
filterExtract f a = (unlines m, catMaybes n)
  where
  (m, n) = unzip . map k . lines $ a
  k a = case f a of
    Nothing -> (a, Nothing)
    Just b  -> ("", Just b)

