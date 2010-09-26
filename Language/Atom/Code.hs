-- | Atom C code generation.
module Language.Atom.Code
  ( Config (..)
  , Clock (..)
  , writeC
  , defaults
  , defaultClock
  , cType
  , RuleCoverage
  ) where

import Data.List
import Data.Maybe
import Text.Printf
import Data.Word

import Data.Generics.Uniplate.Data

import Language.Atom.Analysis
import Language.Atom.Elaboration
import Language.Atom.Expressions
import Language.Atom.Scheduling

-- | C code configuration parameters.
data Config = Config

  { cFuncName     :: String -- ^ Alternative primary function name.  Leave empty
                            -- to use compile name.
  , cStateName    :: String -- ^ Name of state variable structure.  Default: state

  , cCode         :: [Name] -> [Name] -> [(Name, Type)]
                      -> (String, String)  -- ^ Custom C code to insert above
                                           -- and below, given assertion names,
                                           -- coverage names, and probe names
                                           -- and types.
  , cRuleCoverage :: Bool -- ^ Enable rule coverage tracking.
  , cAssert       :: Bool -- ^ Enable assertions and functional coverage.
  , cAssertName   :: String -- ^ Name of assertion function.  Type: void
                            -- assert(int, bool, uint64_t);
  , cCoverName    :: String -- ^ Name of coverage function.  Type: void
                            -- cover(int, bool, uint64_t);
  , hardwareClock :: Maybe Clock -- ^ Do we use a hardware counter to schedule
                                 -- rules?
  }

-- | Data associated with sampling a hardware clock.
data Clock = Clock

  { clockName  :: String        -- ^ C function to sample the clock.  The
                                -- funciton is assumed to have the prototype
                                -- @clockType clockName(void)@.
  , clockType  :: Type          -- ^ Clock type.  Assumed to be one of Word8,
                                -- Word16, Word32, or Word64.  It is permissible
                                -- for the clock to rollover.  
  , delta      :: Integer       -- ^ Number of ticks in a phase.  Must be greater than 0.
  , delay      :: String        -- ^ C function to delay/sleep.  The function is
                                -- assumed to have the prototype @void
                                -- delay(clockType i)@, where @i@ is the
                                -- duration of delay/sleep.
  , err        :: Maybe String  -- ^ Nothing or a user-defined error-reporting
                                -- function if the period duration is violated;
                                -- e.g., the execution time was greater than
                                -- @delta@.  Assumed to have prototype @void
                                -- err(void)@.
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
  , hardwareClock = Nothing
  }

defaultClock :: Clock
defaultClock = Clock "clk" Word64 1 "delay" Nothing

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
-- math.h:
    UPi                  -> [ "M_PI" ]
    UExp   _             -> [ "exp",   f, " ( ", a, " )"]
    ULog   _             -> [ "log",   f, " ( ", a, " )"]
    USqrt  _             -> [ "sqrt",  f, " ( ", a, " )"]
    UPow   _ _           -> [ "pow",   f, " ( ", a, ", ", b, " )"]
    USin   _             -> [ "sin",   f, " ( ", a, " )"]
    UAsin  _             -> [ "asin",  f, " ( ", a, " )"]
    UCos   _             -> [ "cos",   f, " ( ", a, " )"]
    UAcos  _             -> [ "acos",  f, " ( ", a, " )"]
    USinh  _             -> [ "sinh",  f, " ( ", a, " )"]
    UCosh  _             -> [ "cosh",  f, " ( ", a, " )"]
    UAsinh _             -> [ "asinh", f, " ( ", a, " )"]
    UAcosh _             -> [ "acosh", f, " ( ", a, " )"]
    UAtan  _             -> [ "atan",  f, " ( ", a, " )"]
    UAtanh _             -> [ "atanh", f, " ( ", a, " )"]
    where
      ct = cType
      a = head operands
      b = operands !! 1
      c = operands !! 2
      f = case ( typeOf ue ) of
            Float     -> "f"
            Double    -> ""
            _         -> error "unhandled float type"

type RuleCoverage = [(Name, Int, Int)]

containMathHFunctions rules = any isMathHCall ues
       where ues            = rules >>= allUEs >>= universe
             isMathHCall fc = case fc of
                                UPi        -> True
                                UExp   _   -> True
                                ULog   _   -> True
                                USqrt  _   -> True
                                UPow   _ _ -> True
                                USin   _   -> True
                                UAsin  _   -> True
                                UCos   _   -> True
                                UAcos  _   -> True
                                USinh  _   -> True
                                UCosh  _   -> True
                                UAsinh _   -> True
                                UAcosh _   -> True
                                UAtan  _   -> True
                                UAtanh _   -> True
                                _          -> False

writeC :: Name -> Config -> StateHierarchy -> [Rule] -> Schedule -> [Name] 
       -> [Name] -> [(Name, Type)] -> IO RuleCoverage
writeC name config state rules schedule assertionNames coverageNames probeNames = do
  writeFile (name ++ ".c") c
  writeFile (name ++ ".h") h
  return [ (ruleName r, div (ruleId r) 32, mod (ruleId r) 32) | r <- rules' ]
  where
  (preCode, postCode) = cCode config assertionNames coverageNames probeNames
  c = unlines
    [ "#include <stdbool.h>"
    , "#include <stdint.h>"
    , codeIf ( containMathHFunctions rules ) "#include <math.h>"
    , ""
    , preCode
    , ""
    , "static " ++ globalType ++ " " ++ globalClk ++ " = 0;"
    , codeIf (cRuleCoverage config) $ "static const " ++ cType Word32 
                 ++ " __coverage_len = " ++ show covLen ++ ";"
    , codeIf (cRuleCoverage config) $ "static " ++ cType Word32 
                 ++ " __coverage[" ++ show covLen ++ "] = {" 
                 ++ (concat $ intersperse ", " $ replicate covLen "0") ++ "};"
    , codeIf (cRuleCoverage config) $ "static " ++ cType Word32 ++ " __coverage_index = 0;"
    , declState True $ StateHierarchy (cStateName config) [state]
    , concatMap (codeRule config) rules'
    , codeAssertionChecks config assertionNames coverageNames rules
    , "void " ++ funcName ++ "() {"
    , swOrHwClock
    , "}"
    , ""
    , postCode
    ]

  codePeriodPhases = concatMap (codePeriodPhase config) schedule

  swOrHwClock = 
    case hardwareClock config of
      Nothing      -> unlines [codePeriodPhases, "  " ++ globalClk ++ " = " ++ globalClk ++ " + 1;"]
      Just clkData -> unlines 
        [ "  " ++ setGlobalClk clkData
        , ""
        , codePeriodPhases
        , "  // In the following we sample the hardware clock, waiting for the next phase."
        , ""
        , "  " ++ declareConst phaseConst clkDelta
        , "  " ++ declareConst maxConst maxVal
        , "  " ++ globalType ++ " " ++ setTime
        , ""
        , errCheck 
        , "  " ++ setTime ++ " // Update the current time."
        , "  // Wait until the phase has expired.  If the current time hasn't"
        , "  // overflowed, execute the first branch; otherwise, the second."
        , "  if (" ++ currentTime ++ " >= " ++ globalClk ++ ") {"
        , "    " ++ delayFn ++ "(" ++ phaseConst ++ " - (" ++ currentTime
                   ++ " - " ++ globalClk ++ "));" 
        , "  }"
        , "  else {"
        , "    " ++ delayFn ++ "(" ++ phaseConst ++ " - (" ++ currentTime ++ " + (" 
                 ++ maxConst ++ " - " ++ globalClk ++ ")));"
                 
        , "  }"
        ]
        where 
          delayFn = delay clkData
          maxVal :: Integer
          maxVal  = 2 ^ (case clockType clkData of
                           Word8  -> 8
                           Word16 -> 16
                           Word32 -> 32
                           Word64 -> 64
                           _      -> clkTypeErr) - 1
          declareConst varName c = globalType ++ " const " ++ varName 
                                   ++ " = " ++ showConst (constType c) ++ ";"
          setTime     = currentTime ++ " = " ++ clockName clkData ++ "();"
          maxConst    = "__max"
          phaseConst = "__phase_len"
          currentTime = "__curr_time"
          clkDelta | d <= 0 || d > maxVal = 
            error "The delta given for the number of ticks in a phase must be greater than 0."
                   | otherwise = d
            where d = delta clkData
          errCheck = 
            case err clkData of
              Nothing    -> ""
              Just errF  -> unlines 
                [ "  // An error check for when the phase has already expired."
                , "  // The first disjunct is for when the current time has not overflowed,"
                , "  // and the second for when it has."
                , "  if (   ((" ++ currentTime ++ " >= " ++ globalClk ++ ") && (" 
                                ++ currentTime ++ " - " ++ globalClk 
                                ++ " > " ++ phaseConst ++ "))" 
                , "      || (("
                             ++ currentTime ++ " < " ++ globalClk ++ ") && ((" ++ maxConst 
                             ++ " - " ++ globalClk ++ ") + " ++ currentTime ++ " > " 
                             ++ phaseConst ++ "))) {"
                , "    " ++ errF ++ "();"
                , "  }"
                ]
          constType :: Integer -> Const
          constType c = case clockType clkData of
                          Word8  -> CWord8  (fromInteger c :: Word8)
                          Word16 -> CWord16 (fromInteger c :: Word16)
                          Word32 -> CWord32 (fromInteger c :: Word32)
                          Word64 -> CWord64 (fromInteger c :: Word64)
                          _      -> clkTypeErr
                                               
  h = unlines
    [ "#include <stdbool.h>"
    , "#include <stdint.h>"
    , ""
    , "void " ++ funcName ++ "();"
    , ""
    , declState False $ StateHierarchy (cStateName config) [state]
    ]

  globalType = cType (case hardwareClock config of
                        Nothing      -> Word64 -- Default type
                        Just clkData -> case clockType clkData of
                                          Word8  -> Word8
                                          Word16 -> Word16
                                          Word32 -> Word32
                                          Word64 -> Word64
                                          _      -> clkTypeErr)

  clkTypeErr = error "Clock type must be one of Word8, Word16, Word32, Word64."

  funcName = if null (cFuncName config) then name else cFuncName config

  rules' :: [Rule]
  rules' = concat [ r | (_, _, r) <- schedule ]

  covLen = 1 + div (maximum $ map ruleId rules') 32

  setGlobalClk clkData = globalClk ++ " = " ++ clockName clkData ++ "();"

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

globalClk :: String
globalClk = "__global_clock"

codeAssertionChecks :: Config -> [Name] -> [Name] -> [Rule] -> String
codeAssertionChecks config assertionNames coverageNames rules = codeIf (cAssert config) $
  "static void __assertion_checks() {\n" ++
  concatMap (codeUE config ues "  ") ues ++
  concat [ "  if (" ++ id enable ++ ") " ++ cAssertName config ++ "(" ++ assertionId name ++ ", " ++ id check ++ ", " ++ globalClk ++ ");\n" | Assert name enable check <- rules ] ++
  concat [ "  if (" ++ id enable ++ ") " ++ cCoverName  config ++ "(" ++ coverageId  name ++ ", " ++ id check ++ ", " ++ globalClk ++ ");\n" | Cover  name enable check <- rules ] ++
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

