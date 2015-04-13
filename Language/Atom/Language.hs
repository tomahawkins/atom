-- | 
-- Module: Language
-- Description: Definitions for the language/EDSL itself
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Definitions for the Atom EDSL itself

module Language.Atom.Language
  (
    module Language.Atom.Expressions
  -- * Primary Language Containers
  , Atom
  -- * Hierarchical Rule Declarations
  , atom
  , period
  , getPeriod
  , phase
  , exactPhase
  , getPhase
  -- * Action Directives
  , cond
  , Assign (..)
  , incr
  , decr
  -- * Variable Declarations
  , var
  , var'
  , array
  , array'
  , bool
  , bool'
  , int8
  , int8'
  , int16
  , int16'
  , int32
  , int32'
  , int64
  , int64'
  , word8
  , word8'
  , word16
  , word16'
  , word32
  , word32'
  , word64
  , word64'
  , float
  , float'
  , double
  , double'
  -- * Custom Actions
  , action
  , call
  -- * Probing
  , probe
  , probes
  -- * Assertions and Functional Coverage
  , assert
  , cover
  , assertImply
  -- * Utilities
  , Name
  , liftIO
  , path
  , clock
  -- * Code Coverage
  , nextCoverage
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Int
import Data.Word
import Data.List (foldl')

import Language.Atom.Elaboration hiding (Atom)
import qualified Language.Atom.Elaboration as E
import Language.Atom.Expressions
import Language.Atom.UeMap hiding (typeOf)

infixr 1 <==

-- | The Atom monad captures variable and transition rule declarations.
type Atom = E.Atom

-- | Creates a hierarchical node, where each node could be an atomic rule.
atom :: Name -> Atom a -> Atom a
atom name design = do
  name' <- addName name
  (st1, (g1, parent)) <- get
  (a, (st2, (g2, child))) <- liftIO $
                             buildAtom st1 g1 { gState = [] } name' design
  put (st2, ( g2 { gState = gState g1 ++ [StateHierarchy name $ gState g2] }
            , parent { atomSubs = atomSubs parent ++ [child] }))
  return a

-- | Defines the period of execution of sub-rules as a factor of the base rate
-- of the system.  Rule period is bound by the closest period assertion.  For
-- example:
-- > period 10 $ period 2 a   -- Rules in 'a' have a period of 2, not 10.
period :: Int -> Atom a -> Atom a
period n _ | n <= 0 = error "ERROR: Execution period must be greater than 0."
period n atom' = do
  (st, (g, a)) <- get
  put (st, (g { gPeriod = n }, a))
  r <- atom'
  (st', (g', a')) <- get
  put (st', (g' { gPeriod = gPeriod g }, a'))
  return r

-- | Returns the execution period of the current scope.
getPeriod :: Atom Int
getPeriod = do
  (_, (g, _)) <- get
  return $ gPeriod g

phase' :: (Int -> Phase) -> Int -> Atom a -> Atom a
phase' _ n _ | n < 0 = error $ "ERROR: phase " ++ show n ++ " must be at least 0."
phase' phType n atom' = do
  (st, (g, a)) <- get
  if (n >= gPeriod g) 
    then error $ "ERROR: phase " ++ show n ++ " must be less than the current period "
               ++ show (gPeriod g) ++ "."
    else do put (st, (g { gPhase = phType n }, a))
            r <- atom'
            (st', (g', a')) <- get
            put (st', (g' { gPhase = gPhase g }, a'))
            return r
    -- XXX
    -- else do put (g { gPhase = n }, a)
    --         r <- atom
    --         (g', a) <- get
    --         put (g' { gPhase = gPhase g }, a)
    --         return r

-- | Defines the earliest phase within the period at which the rule should
-- execute; the scheduler attempt to find an optimal phase from 0 <= @n@ <
-- period (thus, the 'phase' must be at least zero and less than the current
-- 'period'.).
phase :: Int -> Atom a -> Atom a
phase n a = phase' MinPhase n a

-- | Ensures an atom is scheduled only at phase @n@.
exactPhase :: Int -> Atom a -> Atom a
exactPhase n a = phase' ExactPhase n a

-- | Returns the phase of the current scope.
getPhase :: Atom Int
getPhase = do
  (_, (g, _)) <- get
  return $ case gPhase g of
             MinPhase ph   -> ph
             ExactPhase ph -> ph

-- | Returns the current atom hierarchical path.
path :: Atom String
path = do
  (_, (_, atom')) <- get
  return $ atomName atom'

-- | Local boolean variable declaration.
bool :: Name -> Bool -> Atom (V Bool)
bool = var

-- | External boolean variable declaration.
bool' :: Name -> V Bool
bool' name = var' name Bool

-- | Local int8 variable declaration.
int8 :: Name -> Int8 -> Atom (V Int8)
int8 = var

-- | External int8 variable declaration.
int8' :: Name -> V Int8
int8' name = var' name Int8

-- | Local int16 variable declaration.
int16 :: Name -> Int16 -> Atom (V Int16)
int16 = var

-- | External int16 variable declaration.
int16' :: Name -> V Int16
int16' name = var' name Int16

-- | Local int32 variable declaration.
int32 :: Name -> Int32 -> Atom (V Int32)
int32 = var

-- | External int32 variable declaration.
int32' :: Name -> V Int32
int32' name = var' name Int32

-- | Local int64 variable declaration.
int64 :: Name -> Int64 -> Atom (V Int64)
int64 = var

-- | External int64 variable declaration.
int64' :: Name -> V Int64
int64' name = var' name Int64

-- | Local word8 variable declaration.
word8 :: Name -> Word8 -> Atom (V Word8)
word8 = var

-- | External word8 variable declaration.
word8' :: Name -> V Word8
word8' name = var' name Word8

-- | Local word16 variable declaration.
word16 :: Name -> Word16 -> Atom (V Word16)
word16 = var

-- | External word16 variable declaration.
word16' :: Name -> V Word16
word16' name = var' name Word16

-- | Local word32 variable declaration.
word32 :: Name -> Word32 -> Atom (V Word32)
word32 = var

-- | External word32 variable declaration.
word32' :: Name -> V Word32
word32' name = var' name Word32

-- | Local word64 variable declaration.
word64 :: Name -> Word64 -> Atom (V Word64)
word64 = var

-- | External word64 variable declaration.
word64' :: Name -> V Word64
word64' name = var' name Word64

-- | Local float variable declaration.
float :: Name -> Float -> Atom (V Float)
float = var

-- | External float variable declaration.
float' :: Name -> V Float
float' name = var' name Float

-- | Local double variable declaration.
double :: Name -> Double -> Atom (V Double)
double = var

-- | External double variable declaration.
double' :: Name -> V Double
double' name = var' name Double

-- | Declares an action, which executes C code that is optionally passed
-- some parameters.
action :: ([String] -> String) -- ^ A function which receives a list of
                               -- C parameters, and returns C code that
                               -- should be executed.
          -> [UE] -- ^ A list of expressions; the supplied functions receive
                  -- parameters which correspond to these expressions.
          -> Atom ()
action f ues = do
  (st, (g, a)) <- get
  let (st', hashes) =
        foldl' (\(accSt,hs) ue' ->
                 let (h,accSt') = newUE ue' accSt in (accSt',h:hs))
        (st,[]) ues
  put (st', (g, a { atomActions = atomActions a ++ [(f, hashes)] }))

-- | Calls an external C function of type 'void f(void)'.
call :: Name -- ^ Function @f@
        -> Atom ()
call n = action (\ _ -> n ++ "()") []

-- | Declares a probe. A probe allows inspecting any expression, remotely to
-- its context, at any desired rate.
probe :: Expr a => Name -- ^ Human-readable probe name
         -> E a -- ^ Expression to inspect
         -> Atom ()
probe name a = do
  (st, (g, atom')) <- get
  let (h,st') = newUE (ue a) st
  if any (\ (n, _) -> name == n) $ gProbes g
    then error $ "ERROR: Duplicated probe name: " ++ name
    else put (st', (g { gProbes = (name, h) : gProbes g }, atom'))

-- | Fetches all declared probes to current design point.  The list contained
-- therein is (probe name, untyped expression).
-- See 'Language.Atom.Unit.printProbe'.
probes :: Atom [(String, UE)]
probes = do
  (st, (g, _)) <- get
  let (strs,hs) = unzip (gProbes g)
  let g' = zip strs (map (recoverUE st) hs)
  return g'

-- | Increments a 'NumE' 'V'.
incr :: (Assign a, NumE a) => V a -> Atom ()
incr a = a <== value a + 1

-- | Decrements a 'NumE' 'V'.
decr :: (Assign a, NumE a) => V a -> Atom ()
decr a = a <== value a - 1


class Expr a => Assign a where
  -- | Assign an 'E' to a 'V'.
  (<==) :: V a -> E a -> Atom ()
  v <== e = do
    (st, (g, atom')) <- get
    let (h,st0) = newUE (ue e) st
    let (muv,st1) = newUV (uv v) st0
    put (st1, (g, atom' { atomAssigns = (muv, h) : atomAssigns atom' }))

instance Assign Bool
instance Assign Int8
instance Assign Int16
instance Assign Int32
instance Assign Int64
instance Assign Word8
instance Assign Word16
instance Assign Word32
instance Assign Word64
instance Assign Float
instance Assign Double

-- | Adds an enabling condition to an atom subtree of rules.
-- This condition must be true before any rules in hierarchy
-- are allowed to execute.
cond :: E Bool -> Atom ()
cond c = do
  (st, (g, atom')) <- get
  let ae = recoverUE st (atomEnable atom')
  let (h,st') = newUE (uand ae (ue c)) st
  put (st', (g, atom' { atomEnable = h}))

-- | Reference to the 64-bit free running clock.
clock :: E Word64
clock = value $ word64' "__global_clock"

-- | Rule coverage information.  (current coverage index, coverage data)
nextCoverage :: Atom (E Word32, E Word32)
nextCoverage = do
  action (const "__coverage_index = (__coverage_index + 1) % __coverage_len") []
  return (value $ word32' "__coverage_index", value $ word32' "__coverage[__coverage_index]")


-- | An assertions checks that an 'E Bool' is true.  Assertions are checked
-- between the execution of every rule.  Parent enabling conditions can
-- disable assertions, but period and phase constraints do not.  Assertion
-- names should be globally unique.
assert :: Name -> E Bool -> Atom ()
assert name check = do
  (st, (g, atom')) <- get
  let names = fst $ unzip $ atomAsserts atom'
  when (elem name names) (liftIO $ putStrLn $ "WARNING: Assertion name already used: " ++ name)
  let (chk,st') = newUE (ue check) st
  put (st', (g, atom' { atomAsserts = (name, chk) : atomAsserts atom' }))

-- | Implication assertions.  Creates an implicit coverage point for the
-- precondition.
assertImply :: Name -> E Bool -> E Bool -> Atom ()
assertImply name a b = do
  assert name $ imply a b
  cover (name ++ "Precondition") a

-- | A functional coverage point tracks if an event has occured (true).
-- Coverage points are checked at the same time as assertions.
-- Coverage names should be globally unique.
cover :: Name -> E Bool -> Atom ()
cover name check = do
  (st, (g, atom')) <- get
  let names = fst $ unzip $ atomCovers atom'
  when (elem name names) (liftIO $ putStrLn $ "WARNING: Coverage name already used: " ++ name)
  let (chk,st') = newUE (ue check) st
  put (st', (g, atom' { atomCovers = (name, chk) : atomCovers atom' }))

