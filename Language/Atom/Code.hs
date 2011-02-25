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
import qualified Data.IntMap as M

import Language.Atom.Analysis
import Language.Atom.Elaboration
import Language.Atom.Expressions hiding (typeOf)
import qualified Language.Atom.Expressions as E
import Language.Atom.Scheduling 
import Language.Atom.UeMap

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

-- | Data associated with sampling a hardware clock.  For the clock to work
-- correctly, you MUST assign @__global_clock@ the current time (accoring to 
-- @clockName@) the first time you enter the main Atom-generated function
-- calling your rules.
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

codeUE :: UeMap -> Config -> [(Hash, String)] -> String -> (Hash, String) -> String
codeUE mp config ues d (ue, n) = 
  d ++ cType (typeOf ue mp) ++ " " ++ n ++ " = " ++ basic ++ ";\n"
  where
  operands = map (fromJust . flip lookup ues) $ ueUpstream ue mp
  basic :: String
  basic = concat $ case getUE ue mp of
    MUVRef (MUV _ n _)                 -> [cStateName config, ".", n]
    MUVRef (MUVArray (UA _ n _) _)     -> [cStateName config, ".", n, "[", a, "]"]
    MUVRef (MUVArray (UAExtern n _) _) -> [n, "[", a, "]"]
    MUVRef (MUVExtern n _)             -> [n]
    MUCast _ _            -> ["(", cType (typeOf ue mp), ") ", a]
    MUConst c             -> [showConst c]
    MUAdd _ _             -> [a, " + ", b]
    MUSub _ _             -> [a, " - ", b]
    MUMul _ _             -> [a, " * ", b]
    MUDiv _ _             -> [a, " / ", b]
    MUMod _ _             -> [a, " % ", b]
    MUNot _               -> ["! ", a]
    MUAnd _               -> intersperse " && " operands
    MUBWNot _             -> ["~ ", a]
    MUBWAnd _ _           -> [a, " & ", b]
    MUBWOr  _ _           -> [a, " | ", b]
    MUShift _ n           -> (if n >= 0 then [a, " << ", show n] else [a, " >> ", show (negate n)])
    MUEq  _ _             -> [a, " == ", b]
    MULt  _ _             -> [a, " < " , b]
    MUMux _ _ _           -> [a, " ? " , b, " : ", c]
    MUF2B _               -> ["*((", ct Word32, " *) &(", a, "))"]
    MUD2B _               -> ["*((", ct Word64, " *) &(", a, "))"]
    MUB2F _               -> ["*((", ct Float , " *) &(", a, "))"]
    MUB2D _               -> ["*((", ct Double, " *) &(", a, "))"]
-- math.h:
    MUPi                  -> [ "M_PI" ]
    MUExp   _             -> [ "exp",   f, " ( ", a, " )"]
    MULog   _             -> [ "log",   f, " ( ", a, " )"]
    MUSqrt  _             -> [ "sqrt",  f, " ( ", a, " )"]
    MUPow   _ _           -> [ "pow",   f, " ( ", a, ", ", b, " )"]
    MUSin   _             -> [ "sin",   f, " ( ", a, " )"]
    MUAsin  _             -> [ "asin",  f, " ( ", a, " )"]
    MUCos   _             -> [ "cos",   f, " ( ", a, " )"]
    MUAcos  _             -> [ "acos",  f, " ( ", a, " )"]
    MUSinh  _             -> [ "sinh",  f, " ( ", a, " )"]
    MUCosh  _             -> [ "cosh",  f, " ( ", a, " )"]
    MUAsinh _             -> [ "asinh", f, " ( ", a, " )"]
    MUAcosh _             -> [ "acosh", f, " ( ", a, " )"]
    MUAtan  _             -> [ "atan",  f, " ( ", a, " )"]
    MUAtanh _             -> [ "atanh", f, " ( ", a, " )"]
    where
      ct = cType
      a = head operands
      b = operands !! 1
      c = operands !! 2
      f = case ( typeOf ue mp) of
            Float     -> "f"
            Double    -> ""
            _         -> error "unhandled float type"

type RuleCoverage = [(Name, Int, Int)]

-- containMathHFunctions :: [Rule] -> Bool
-- containMathHFunctions rules = 
--   any math rules
--   where math rule = case rule of
--                       Rule _ _ _ _ _ _ _ b -> b
--                       _                    -> False

writeC :: Name -> Config -> StateHierarchy -> [Rule] -> Schedule -> [Name] 
       -> [Name] -> [(Name, Type)] -> IO RuleCoverage
writeC name config state rules (mp, schedule) assertionNames coverageNames probeNames = do
  writeFile (name ++ ".c") c
  writeFile (name ++ ".h") h
  return [ (ruleName r, div (ruleId r) 32, mod (ruleId r) 32) | r <- rules' ]
  where
  (preCode, postCode) = cCode config assertionNames coverageNames probeNames
  c = unlines
    [ "#include <stdbool.h>"
    , "#include <stdint.h>"
    , codeIf (M.fold (\e ans -> isMathHCall e || ans ) False (snd mp)) 
             "#include <math.h>"
    , ""
    , preCode
    , ""
    , "static " ++ globalType ++ " " ++ globalClk ++ " = 0;"
    , codeIf (cRuleCoverage config) $ "static const " ++ cType Word32 
                 ++ " __coverage_len = " ++ show covLen ++ ";"
    , codeIf (cRuleCoverage config) $ "static " ++ cType Word32 
                 ++ " __coverage[" ++ show covLen ++ "] = {" 
                 ++ (concat $ intersperse ", " $ replicate covLen "0") ++ "};"
    , codeIf (cRuleCoverage config) 
             ("static " ++ cType Word32 ++ " __coverage_index = 0;")
    , declState True (StateHierarchy (cStateName config) [state])
    , concatMap (codeRule mp config) rules'
    , codeAssertionChecks mp config assertionNames coverageNames rules
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
        [ ""
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
        , ""
        , "  " ++ setGlobalClk clkData
        ]
        where
          delayFn = delay clkData
          maxVal :: Integer
          maxVal  = case clockType clkData of
                      Word8  -> toInteger (maxBound :: Word8)
                      Word16 -> toInteger (maxBound :: Word16)
                      Word32 -> toInteger (maxBound :: Word32)
                      Word64 -> toInteger (maxBound :: Word64)
                      _      -> clkTypeErr
          declareConst varName c = globalType ++ " const " ++ varName
                                   ++ " = " ++ showConst (constType c) ++ ";"
          setTime     = currentTime ++ " = " ++ clockName clkData ++ "();"
          maxConst    = "__max"
          phaseConst = "__phase_len"
          currentTime = "__curr_time"
          clkDelta | d <= 0
                       = error $ "The delta "
                                 ++ show d
                                 ++ ", given for the number of ticks "
                                 ++ "in a phase must be greater than 0."
                   | d > maxVal
                       = error $ "The delta "
                         ++ show d
                         ++ ", given for the number of ticks in a phase "
                         ++ "must be smaller than "
                         ++ show ( maxVal + 1 )
                         ++ "."
                   | otherwise
                       = d
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
    , declState False (StateHierarchy (cStateName config) [state])
    ]

  globalType = cType (case hardwareClock config of
                        Nothing      -> Word64 -- Default type
                        Just clkData -> case clockType clkData of
                                          Word8  -> Word8
                                          Word16 -> Word16
                                          Word32 -> Word32
                                          Word64 -> Word64
                                          _      -> clkTypeErr)

  clkTypeErr :: a
  clkTypeErr = error "Clock type must be one of Word8, Word16, Word32, Word64."

  funcName = if null (cFuncName config) then name else cFuncName config

  rules' :: [Rule]
  rules' = concat [ r | (_, _, r) <- schedule ]

  covLen = 1 + div (maximum $ map ruleId rules') 32

  setGlobalClk clkData = globalClk ++ " = " ++ clockName clkData ++ "();"

codeIf :: Bool -> String -> String
codeIf a b = if a then b else ""

declState :: Bool -> StateHierarchy -> String
declState define a = 
     (if define then "" else "extern ") ++ init (init (f1 "" a)) 
  ++ (if define then " =\n" ++ f2 "" a else "") ++ ";\n"
  where
  f1 i a = case a of
    StateHierarchy name items -> 
         i ++ "struct {  /* " ++ name ++ " */\n" 
      ++ concatMap (f1 ("  " ++ i)) items ++ i ++ "} " ++ name ++ ";\n"
    StateVariable  name c     -> i ++ cType (E.typeOf c) ++ " " ++ name ++ ";\n"
    StateArray     name c     -> 
      i ++ cType (E.typeOf $ head c) ++ " " ++ name ++ "[" ++ show (length c) ++ "];\n"

  f2 i a = case a of
    StateHierarchy name items -> 
         i ++ "{  /* " ++ name ++ " */\n" 
      ++ intercalate ",\n" (map (f2 ("  " ++ i)) items) ++ "\n" ++ i ++ "}"
    StateVariable  name c     -> i ++ "/* " ++ name ++ " */  " ++ showConst c
    StateArray     name c     -> 
         i ++ "/* " ++ name ++ " */\n" ++ i ++ "{ " 
      ++ intercalate ("\n" ++ i ++ ", ") (map showConst c) ++ "\n" ++ i ++ "}"

codeRule :: UeMap -> Config -> Rule -> String
codeRule mp config rule@(Rule _ _ _ _ _ _ _) =
  "/* " ++ show rule ++ " */\n" ++
  "static void __r" ++ show (ruleId rule) ++ "() {\n" ++
  concatMap (codeUE mp config ues "  ") ues ++
  "  if (" ++ id (ruleEnable rule) ++ ") {\n" ++
  concatMap codeAction (ruleActions rule) ++
  codeIf (cRuleCoverage config) 
         ( "    __coverage[" ++ covWord ++ "] = __coverage[" ++ covWord 
          ++ "] | (1 << " ++ covBit ++ ");\n") 
  ++ "  }\n" ++ concatMap codeAssign (ruleAssigns rule) ++ "}\n\n"
  where
  ues = topo mp $ allUEs rule
  id ue = fromJust $ lookup ue ues

  codeAction :: (([String] -> String), [Hash]) -> String
  codeAction (f, args) = "    " ++ f (map id args) ++ ";\n"

  covWord = show $ div (ruleId rule) 32
  covBit  = show $ mod (ruleId rule) 32

  codeAssign :: (MUV, Hash) -> String
  codeAssign (uv, ue) = concat ["  ", lh, " = ", id ue, ";\n"]
    where
    lh = case uv of
      MUV _ n _                     -> concat [cStateName config, ".", n]
      MUVArray (UA _ n _)     index -> 
        concat [cStateName config, ".", n, "[", id index, "]"]
      MUVArray (UAExtern n _) index -> concat [n, "[", id index, "]"]
      MUVExtern n _                 -> n

codeRule _ _ _ = ""

globalClk :: String
globalClk = "__global_clock"

codeAssertionChecks :: UeMap -> Config -> [Name] -> [Name] -> [Rule] -> String
codeAssertionChecks mp config assertionNames coverageNames rules = 
  codeIf (cAssert config) $
  "static void __assertion_checks() {\n" ++
  concatMap (codeUE mp config ues "  ") ues ++
  concat [     "  if (" ++ id enable ++ ") " ++ cAssertName config 
            ++ "(" ++ assertionId name ++ ", " ++ id check ++ ", " 
            ++ globalClk ++ ");\n"                                 
          | Assert name enable check <- rules ] ++
  concat [     "  if (" ++ id enable ++ ") " ++ cCoverName  config 
            ++ "(" ++ coverageId  name ++ ", " ++ id check ++ ", " 
            ++ globalClk ++ ");\n"                                 
          | Cover  name enable check <- rules ] ++
  "}\n\n"
  where
  ues = topo mp $    concat [ [a, b] | Assert _ a b <- rules ] 
                  ++ concat [ [a, b] | Cover _ a b <- rules ]
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

