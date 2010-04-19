-- | Atom C code generation.
module Language.Atom.Code
  ( Config (..)
  , writeC
  , defaults
  , cType
  , RuleCoverage
  ) where

import Data.List
import Data.Maybe
import Text.Printf

import Language.Atom.Analysis
import Language.Atom.Elaboration
import Language.Atom.Expressions
import Language.Atom.Scheduling

-- | C code configuration parameters.
data Config = Config
  { cFuncName     :: String                                                  -- ^ Alternative primary function name.  Leave empty to use compile name.
  , cStateName    :: String                                                  -- ^ Name of state variable structure.  Default: state
  , cCode         :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)  -- ^ Custom C code to insert above and below, given assertion names, coverage names, and probe names and types.
  , cRuleCoverage :: Bool                                                    -- ^ Enable rule coverage tracking.
  , cAssert       :: Bool                                                    -- ^ Enable assertions and functional coverage.
  , cAssertName   :: String                                                  -- ^ Name of assertion function.  Type: void assert(int, bool, uint64_t);
  , cCoverName    :: String                                                  -- ^ Name of coverage function.  Type: void cover(int, bool, uint64_t);
  }

-- | Default C code configuration parameters (default function name, no pre/post code, ANSI C types).
defaults :: Config
defaults = Config
  { cFuncName     = ""
  , cStateName    = "state"
  , cCode         = \ _ _ _ -> ("", "")
  , cRuleCoverage = True
  , cAssert       = True
  , cAssertName   = "assert"
  , cCoverName    = "cover"
  }

showConst :: Const -> String
showConst c = case c of
  CBool True  -> "true"
  CBool False -> "false"
  CInt8   a   -> show a
  CInt16  a   -> show a
  CInt32  a   -> show a ++ "L"
  CInt64  a   -> show a ++ "LL"
  CWord8  a   -> show a
  CWord16 a   -> show a
  CWord32 a   -> show a ++ "UL"
  CWord64 a   -> show a ++ "ULL"
  CFloat  a   -> show a ++ "F"
  CDouble a   -> show a


-- | C99 type naming rules.
cType :: Type -> String
cType t = case t of
  Bool   -> "bool"
  Int8   -> "int8_t"
  Int16  -> "int16_t"
  Int32  -> "int32_t"
  Int64  -> "int64_t"
  Word8  -> "uint8_t"
  Word16 -> "uint16_t"
  Word32 -> "uint32_t"
  Word64 -> "uint64_t"
  Float  -> "float"
  Double -> "double"

codeUE :: Config -> [(UE, String)] -> String -> (UE, String) -> String
codeUE config ues d (ue, n) = d ++ cType (typeOf ue) ++ " " ++ n ++ " = " ++ basic operands ++ ";\n"
  where
  operands = map (fromJust . flip lookup ues) $ ueUpstream ue
  basic :: [String] -> String
  basic operands = concat $ case ue of
    UVRef (UV _ n _)                 -> [cStateName config, ".", n]
    UVRef (UVArray (UA _ n _) _)     -> [cStateName config, ".", n, "[", a, "]"]
    UVRef (UVArray (UAExtern n _) _) -> [n, "[", a, "]"]
    UVRef (UVExtern n _)             -> [n]
    UCast _ _            -> ["(", cType (typeOf ue), ") ", a]
    UConst c             -> [showConst c]
    UAdd _ _             -> [a, " + ", b]
    USub _ _             -> [a, " - ", b]
    UMul _ _             -> [a, " * ", b]
    UDiv _ _             -> [a, " / ", b]
    UMod _ _             -> [a, " % ", b]
    UNot _               -> ["! ", a]
    UAnd _               -> intersperse " && " operands
    UBWNot _             -> ["~ ", a]
    UBWAnd _ _           -> [a, " & ", b]
    UBWOr  _ _           -> [a, " | ", b]
    UShift _ n           -> (if n >= 0 then [a, " << ", show n] else [a, " >> ", show (negate n)])
    UEq  _ _             -> [a, " == ", b]
    ULt  _ _             -> [a, " < " , b]
    UMux _ _ _           -> [a, " ? " , b, " : ", c]
    UF2B _               -> ["*((", ct Word32, " *) &(", a, "))"]
    UD2B _               -> ["*((", ct Word64, " *) &(", a, "))"]
    UB2F _               -> ["*((", ct Float , " *) &(", a, "))"]
    UB2D _               -> ["*((", ct Double, " *) &(", a, "))"]
    where
    ct = cType
    a = head operands
    b = operands !! 1
    c = operands !! 2

type RuleCoverage = [(Name, Int, Int)]

writeC :: Name -> Config -> StateHierarchy -> [Rule] -> Schedule -> [Name] -> [Name] -> [(Name, Type)] -> IO RuleCoverage
writeC name config state rules schedule assertionNames coverageNames probeNames = do
  writeFile (name ++ ".c") c
  writeFile (name ++ ".h") h
  return [ (ruleName r, div (ruleId r) 32, mod (ruleId r) 32) | r <- rules' ]
  where
  (preCode, postCode) = cCode config assertionNames coverageNames probeNames
  c = unlines
    [ "#include <stdbool.h>"
    , "#include <stdint.h>"
    , ""
    , preCode
    , ""
    , "static " ++ cType Word64 ++ " __global_clock = 0;"
    , codeIf (cRuleCoverage config) $ "static const " ++ cType Word32 ++ " __coverage_len = " ++ show covLen ++ ";"
    , codeIf (cRuleCoverage config) $ "static " ++ cType Word32 ++ " __coverage[" ++ show covLen ++ "] = {" ++ (concat $ intersperse ", " $ replicate covLen "0") ++ "};"
    , codeIf (cRuleCoverage config) $ "static " ++ cType Word32 ++ " __coverage_index = 0;"
    , declState True $ StateHierarchy (cStateName config) [state]
    , concatMap (codeRule config) rules'
    , codeAssertionChecks config assertionNames coverageNames rules
    , "void " ++ funcName ++ "() {"
    , concatMap (codePeriodPhase config) schedule
    , "  __global_clock = __global_clock + 1;"
    , "}"
    , ""
    , postCode
    ]

  h = unlines
    [ "#include <stdbool.h>"
    , "#include <stdint.h>"
    , ""
    , "void " ++ funcName ++ "();"
    , ""
    , declState False $ StateHierarchy (cStateName config) [state]
    ]

  funcName = if null (cFuncName config) then name else cFuncName config

  rules' :: [Rule]
  rules' = concat [ r | (_, _, r) <- schedule ]

  covLen = 1 + div (maximum $ map ruleId rules') 32

codeIf :: Bool -> String -> String
codeIf a b = if a then b else ""

declState :: Bool -> StateHierarchy -> String
declState define a = (if define then "" else "extern ") ++ init (init (f1 "" a)) ++ (if define then " =\n" ++ f2 "" a else "") ++ ";\n"
  where
  f1 i a = case a of
    StateHierarchy name items -> i ++ "struct {  /* " ++ name ++ " */\n" ++ concatMap (f1 ("  " ++ i)) items ++ i ++ "} " ++ name ++ ";\n"
    StateVariable  name c     -> i ++ cType (typeOf c) ++ " " ++ name ++ ";\n"
    StateArray     name c     -> i ++ cType (typeOf $ head c) ++ " " ++ name ++ "[" ++ show (length c) ++ "];\n"

  f2 i a = case a of
    StateHierarchy name items -> i ++ "{  /* " ++ name ++ " */\n" ++ intercalate ",\n" (map (f2 ("  " ++ i)) items) ++ "\n" ++ i ++ "}"
    StateVariable  name c     -> i ++ "/* " ++ name ++ " */  " ++ showConst c
    StateArray     name c     -> i ++ "/* " ++ name ++ " */\n" ++ i ++ "{ " ++ intercalate ("\n" ++ i ++ ", ") (map showConst c) ++ "\n" ++ i ++ "}"

codeRule :: Config -> Rule -> String
codeRule config rule@(Rule _ _ _ _ _ _ _) =
  "/* " ++ show rule ++ " */\n" ++
  "static void __r" ++ show (ruleId rule) ++ "() {\n" ++
  concatMap (codeUE config ues "  ") ues ++
  "  if (" ++ id (ruleEnable rule) ++ ") {\n" ++
  concatMap codeAction (ruleActions rule) ++
  codeIf (cRuleCoverage config) ("    __coverage[" ++ covWord ++ "] = __coverage[" ++ covWord ++ "] | (1 << " ++ covBit ++ ");\n") ++
  "  }\n" ++
  concatMap codeAssign (ruleAssigns rule) ++
  "}\n\n"
  where
  ues = topo $ allUEs rule
  id ue = fromJust $ lookup ue ues

  codeAction :: (([String] -> String), [UE]) -> String
  codeAction (f, args) = "    " ++ f (map id args) ++ ";\n"

  covWord = show $ div (ruleId rule) 32
  covBit  = show $ mod (ruleId rule) 32

  codeAssign :: (UV, UE) -> String
  codeAssign (uv, ue) = concat ["  ", lh, " = ", id ue, ";\n"]
    where
    lh = case uv of
      UV _ n _                     -> concat [cStateName config, ".", n]
      UVArray (UA _ n _)     index -> concat [cStateName config, ".", n, "[", id index, "]"]
      UVArray (UAExtern n _) index -> concat [n, "[", id index, "]"]
      UVExtern n _                 -> n

codeRule _ _ = ""

codeAssertionChecks :: Config -> [Name] -> [Name] -> [Rule] -> String
codeAssertionChecks config assertionNames coverageNames rules = codeIf (cAssert config) $
  "static void __assertion_checks() {\n" ++
  concatMap (codeUE config ues "  ") ues ++
  concat [ "  if (" ++ id enable ++ ") " ++ cAssertName config ++ "(" ++ assertionId name ++ ", " ++ id check ++ ", __global_clock);\n" | Assert name enable check <- rules ] ++
  concat [ "  if (" ++ id enable ++ ") " ++ cCoverName  config ++ "(" ++ coverageId  name ++ ", " ++ id check ++ ", __global_clock);\n" | Cover  name enable check <- rules ] ++
  "}\n\n"
  where
  ues = topo $ concat [ [a, b] | Assert _ a b <- rules ] ++ concat [ [a, b] | Cover _ a b <- rules ]
  id ue = fromJust $ lookup ue ues
  assertionId :: Name -> String
  assertionId name = show $ fromJust $ elemIndex name assertionNames
  coverageId :: Name -> String
  coverageId name = show $ fromJust $ elemIndex name coverageNames

codePeriodPhase :: Config -> (Int, Int, [Rule]) -> String
codePeriodPhase config (period, phase, rules) = unlines
  [ printf "  {"
  , printf "    static %s __scheduling_clock = %i;" (cType clockType) phase
  , printf "    if (__scheduling_clock == 0) {"
  , intercalate "\n" $ map callRule rules
  , printf "      __scheduling_clock = %i;" (period - 1)
  , printf "    }"
  , printf "    else {"
  , printf "      __scheduling_clock = __scheduling_clock - 1;"
  , printf "    }"
  , printf "  }"
  ]
  where
  clockType | period < 2 ^  8 = Word8
            | period < 2 ^ 16 = Word16
            | otherwise       = Word32
  callRule r = concat ["      ", codeIf (cAssert config) "__assertion_checks(); ", "__r", show (ruleId r), "();  /* ", show r, " */"]

