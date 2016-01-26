-- | 
-- Module: Elaboration
-- Description: -
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike

module Language.Atom.Elaboration
  (
--    UeStateT
  -- * Atom monad and container.
    Atom
  , AtomDB     (..)
  , Global     (..)
  , Rule       (..)
  , StateHierarchy (..)
  , buildAtom
  -- * Type Aliases and Utilities
  , UID
  , Name
  , Phase (..)
  , Path
  , elaborate
  , var
  , var'
  , array
  , array'
  , addName
  , get
  , put
  , allUVs
  , allUEs
  ) where

import Control.Monad (ap)
import Control.Monad.Trans
import Data.Function (on)
import Data.List
import Data.Char
import qualified Control.Monad.State.Strict as S

import Language.Atom.Expressions hiding (typeOf)
import Language.Atom.UeMap


type UID = Int

-- | A name.
type Name = String

-- | A hierarchical name.
type Path = [Name]

-- | A phase is either the minimum phase or the exact phase.
data Phase = MinPhase Int | ExactPhase Int

data Global = Global
  { gRuleId  :: Int
  , gVarId   :: Int
  , gArrayId :: Int
  , gState   :: [StateHierarchy]
  , gProbes  :: [(String, Hash)]
  , gPeriod  :: Int
  , gPhase   :: Phase
  }

data AtomDB = AtomDB
  { atomId          :: Int
  , atomName        :: Name
  , atomNames       :: [Name]      -- Names used at this level.
  , atomEnable      :: Hash        -- Enabling condition.
  , atomSubs        :: [AtomDB]    -- Sub atoms.
  , atomPeriod      :: Int
  , atomPhase       :: Phase
  , atomAssigns     :: [(MUV, Hash)]
  , atomActions     :: [([String] -> String, [Hash])]
  , atomAsserts     :: [(Name, Hash)]
  , atomCovers      :: [(Name, Hash)]
  }

data Rule
  = Rule
    { ruleId        :: Int
    , ruleName      :: Name
    , ruleEnable    :: Hash
    , ruleAssigns   :: [(MUV, Hash)]
    , ruleActions   :: [([String] -> String, [Hash])]
    , rulePeriod    :: Int
    , rulePhase     :: Phase
--    , mathH         :: Bool -- Contains a math.h call?
    }
  | Assert
    { ruleName      :: Name
    , ruleEnable    :: Hash
    , ruleAssert    :: Hash
    }
  | Cover
    { ruleName      :: Name
    , ruleEnable    :: Hash
    , ruleCover     :: Hash
    }

data StateHierarchy
  = StateHierarchy Name [StateHierarchy]
  | StateVariable  Name Const
  | StateArray     Name [Const]

instance Show AtomDB where show = atomName
instance Eq   AtomDB where (==) = (==) `on` atomId
instance Ord  AtomDB where compare a b = compare (atomId a) (atomId b)
instance Show Rule   where show = ruleName

elaborateRules:: Hash -> AtomDB -> UeState [Rule]
elaborateRules parentEnable atom = 
  if isRule 
    then do r <- rule
            rs <- rules 
            return $ r : rs 
    else rules
  where
  isRule = not $ null (atomAssigns atom) && null (atomActions atom)
  enable :: UeState Hash
  enable = do
    st <- S.get
    let (h,st') = newUE (uand (recoverUE st parentEnable)
                              (recoverUE st (atomEnable atom)))
                         st
    S.put st'
    return h
  rule :: UeState Rule
  rule = do 
    h <- enable 
    assigns <- S.foldM (\prs pr -> do pr' <- enableAssign pr                                      
                                      return $ pr' : prs) [] 
                       (atomAssigns atom)
--    st <- S.get
    return $ Rule
      { ruleId        = atomId   atom
      , ruleName      = atomName atom
      , ruleEnable    = h
      , ruleAssigns   = assigns
      , ruleActions   = atomActions atom
      , rulePeriod    = atomPeriod  atom
      , rulePhase     = atomPhase   atom
      }
  assert :: (Name, Hash) -> UeState Rule
  assert (name, u) = do 
    h <- enable 
    return $ Assert
      { ruleName      = name
      , ruleEnable    = h
      , ruleAssert    = u
      }
  cover :: (Name, Hash) -> UeState Rule
  cover (name, u) = do 
    h <- enable 
    return $ Cover
      { ruleName      = name
      , ruleEnable    = h
      , ruleCover     = u
      }
  rules :: UeState [Rule]
  rules = do
    asserts <- S.foldM (\rs e -> do r <- assert e
                                    return $ r:rs
                       ) [] (atomAsserts atom) 
    covers  <- S.foldM (\rs e -> do r <- cover e
                                    return $ r:rs
                       ) [] (atomCovers atom) 
    rules'  <- S.foldM (\rs db -> do en <- enable
                                     r <- elaborateRules en db
                                     return $ r:rs
                       ) [] (atomSubs atom)
    return $ asserts ++ covers ++ concat rules'
  enableAssign :: (MUV, Hash) -> UeState (MUV, Hash)
  enableAssign (uv', ue') = do 
    e <- enable
    h <- maybeUpdate (MUVRef uv')
    st <- S.get 
    let (h',st') = newUE (umux (recoverUE st e) 
                               (recoverUE st ue') 
                               (recoverUE st h))
                         st
    S.put st'
    return (uv', h')

reIdRules :: Int -> [Rule] -> [Rule]
reIdRules _ [] = []
reIdRules i (a:b) = case a of
  Rule _ _ _ _ _ _ _ -> a { ruleId = i } : reIdRules (i + 1) b
  _                    -> a                : reIdRules  i      b

buildAtom :: UeMap -> Global -> Name -> Atom a -> IO (a, AtomSt)
buildAtom st g name (Atom f) = do
  let (h,st') = newUE (ubool True) st
--  S.put st' 
  f (st', (g { gRuleId = gRuleId g + 1 }, AtomDB
                 { atomId        = gRuleId g
                 , atomName      = name
                 , atomNames     = []
                 , atomEnable    = h 
                 , atomSubs      = []
                 , atomPeriod    = gPeriod g
                 , atomPhase     = gPhase  g
                 , atomAssigns   = []
                 , atomActions   = []
                 , atomAsserts   = []
                 , atomCovers    = []
                 }))
--  S.return db

type AtomSt = (UeMap, (Global, AtomDB))

-- | The Atom monad holds variable and rule declarations.
data Atom a = Atom (AtomSt -> IO (a, AtomSt))

instance Applicative Atom where
  pure = return
  (<*>) = ap

instance Functor Atom where
  fmap = S.liftM

instance Monad Atom where
  return a = Atom (\ s -> return (a, s))
  (Atom f1) >>= f2 = Atom f3
    where
    f3 s = do
      (a, s') <- f1 s
      let Atom f4 = f2 a
      f4 s'

instance MonadIO Atom where
  liftIO io = Atom f
    where
    f s = do
      a <- io
      return (a, s)

get :: Atom AtomSt
get = Atom (\ s -> return (s, s))

put :: AtomSt -> Atom ()
put s = Atom (\ _ -> return ((), s))

-- | A Relation is used for relative performance constraints between 'Action's.
-- data Relation = Higher UID | Lower UID deriving (Show, Eq)

-- XXX elaborate is a bit hacky since we're threading state through this
-- function, but I don't want to go change all the UeState monads to UeStateT
-- monads.
-- | Given a top level name and design, elaborates design and returns a design database.
elaborate :: UeMap -> Name -> Atom () 
          -> IO (Maybe ( UeMap
                       , (  StateHierarchy, [Rule], [Name], [Name]
                          , [(Name, Type)])
                       ))
elaborate st name atom = do
  (_, (st0, (g, atomDB))) <- buildAtom st Global { gRuleId = 0
                                                 , gVarId = 0
                                                 , gArrayId = 0
                                                 , gState = []
                                                 , gProbes = []
                                                 , gPeriod = 1
                                                 , gPhase  = MinPhase 0 
                                                 } 
                                       name atom 
  let (h,st1) = newUE (ubool True) st0
      (getRules,st2) = S.runState (elaborateRules h atomDB) st1
      rules = reIdRules 0 (reverse getRules)
      coverageNames  = [ name' | Cover  name' _ _ <- rules ]
      assertionNames = [ name' | Assert name' _ _ <- rules ]
      probeNames = [ (n, typeOf a st2) | (n, a) <- gProbes g ]
  if (null rules) 
    then do
      putStrLn "ERROR: Design contains no rules.  Nothing to do."
      return Nothing
    else do
      mapM_ (checkEnable st2) rules
      ok <- mapM checkAssignConflicts rules
      return (if and ok 
                then Just ( st2
                          , (trimState $ StateHierarchy name 
                              $ gState g, rules, assertionNames
                                  , coverageNames, probeNames))
                else Nothing)

trimState :: StateHierarchy -> StateHierarchy
trimState a = case a of
  StateHierarchy name items -> 
    StateHierarchy name $ filter f $ map trimState items 
  a' -> a'
  where
  f (StateHierarchy _ []) = False
  f _ = True


-- | Checks that a rule will not be trivially disabled.
checkEnable :: UeMap -> Rule -> IO ()
checkEnable st rule 
  | ruleEnable rule == (fst $ newUE (ubool False) st) = 
      putStrLn $ "WARNING: Rule will never execute: " ++ show rule
  | otherwise                      = return ()

-- | Check that a variable is assigned more than once in a rule.  Will
-- eventually be replaced consistent assignment checking.
checkAssignConflicts :: Rule -> IO Bool
checkAssignConflicts rule@(Rule _ _ _ _ _ _ _) = 
  if length vars /= length vars'
    then do
      putStrLn $ "ERROR: Rule " 
                   ++ show rule 
                   ++ " contains multiple assignments to the same variable(s)."
      return False
    else do
      return True
  where
  vars = fst $ unzip $ ruleAssigns rule
  vars' = nub vars
checkAssignConflicts _ = return True

{-
-- | Checks that all array indices are not a function of array variables.
checkArrayIndices :: [Rule] -> Rule -> IO Bool
checkArrayIndices rules rule =
  where
  ues = allUEs rule
  arrayIndices' = concatMap arrayIndices ues
  [ (name, ) | (UA _ name _, index) <- concatMap arrayIndices ues, UV (Array (UA _ name' init)) <- allUVs rules index, length init /= 1 ]

data UA = UA Int String [Const] deriving (Show, Eq, Ord)
data UV = UV UVLocality deriving (Show, Eq, Ord)
data UVLocality = Array UA UE | External String Type deriving (Show, Eq, Ord)

  allUVs :: [Rule] -> UE -> [UV]
  arrayIndices :: UE -> [(UA, UE)]


  , ruleEnable    :: UE
  , ruleAssigns   :: [(UV, UE)]
  , ruleActions   :: [([String] -> String, [UE])]
-}



-- | Generic local variable declaration.
var :: Expr a => Name -> a -> Atom (V a)
var name init' = do
  name' <- addName name
  (st, (g, atom)) <- get
  let uv' = UV (gVarId g) name' c
      c = constant init'
  put (st, (g { gVarId = gVarId g + 1, gState = gState g ++ [StateVariable name c] }, atom))
  return $ V uv'

-- | Generic external variable declaration.
var' :: Name -> Type -> V a
var' name t = V $ UVExtern name t

-- | Generic array declaration.
array :: Expr a => Name -> [a] -> Atom (A a)
array name [] = error $ "ERROR: arrays can not be empty: " ++ name
array name init' = do
  name' <- addName name
  (st, (g, atom)) <- get
  let ua = UA (gArrayId g) name' c
      c = map constant init'
  put (st, (g { gArrayId = gArrayId g + 1, gState = gState g ++ [StateArray name c] }, atom))
  return $ A ua

-- | Generic external array declaration.
array' :: Expr a => Name -> Type -> A a
array' name t = A $ UAExtern  name t

addName :: Name -> Atom Name
addName name = do
  (st, (g, atom)) <- get
  checkName name
  if elem name (atomNames atom)
    then error $ "ERROR: Name \"" ++ name ++ "\" not unique in " ++ show atom ++ "."
    else do
      put (st, (g, atom { atomNames = name : atomNames atom }))
      return $ atomName atom ++ "." ++ name

-- still accepts some misformed names, like "_.." or "_]["
checkName :: Name -> Atom ()
checkName name =
  if (\ x -> isAlpha x || x == '_') (head name) &&
      and (map (\ x -> isAlphaNum x || x `elem` "._[]") (tail name))
    then return ()
    else error $ "ERROR: Name \"" ++ name ++ "\" is not a valid identifier."

{-
ruleGraph :: Name -> [Rule] -> [UV] -> IO ()
ruleGraph name rules uvs = do
  putStrLn $ "Writing rule graph (" ++ name ++ ".dot)..."
  writeFile (name ++ ".dot") g
  --system $ "dot -o " ++ name ++ ".png -Tpng " ++ name ++ ".dot"
  return ()
  where
  adminUVs =
    [ UV (-1) "__clock" (External Word64)
    , UV (-2) "__coverage_index" (External Word32)
    , UV (-3) "__coverage[__coverage_index]" (External Word32)
    ]

  g = unlines
    [ "digraph " ++ name ++ "{"
    , concat [ "  r" ++ show (ruleId r) ++ " [label = \"" ++ show r ++ "\" shape = ellipse];\n" | r <- rules ]
    , concat [ "  v" ++ show i ++ " [label = \"" ++ n ++ "\" shape = box];\n" | (UV i n _) <- adminUVs ++ uvs ]
    , concat [ "  r" ++ show (ruleId r) ++ " -> v" ++ show i ++ "\n" | r <- rules,  (UV i _ _, _) <- ruleAssigns r ]
    , concat [ "  v" ++ show i ++ " -> r" ++ show (ruleId r) ++ "\n" | r <- rules,  (UV i _ _) <- ruleUVRefs r ]
    , "}"
    ]

  ruleUVRefs r = nub $ concatMap uvSet ues
    where
    ues = ruleEnable r : snd (unzip (ruleAssigns r)) ++ concat (snd (unzip (ruleActions r)))
-}

-- | All the variables that directly and indirectly control the value of an expression.
allUVs :: UeMap -> [Rule] -> Hash -> [MUV]
allUVs st rules ue' = fixedpoint next $ nearestUVs ue' st
  where
  assigns = concat [ ruleAssigns r | r@(Rule _ _ _ _ _ _ _) <- rules ]
  previousUVs :: MUV -> [MUV]
  previousUVs u = concat [ nearestUVs ue_ st | (uv', ue_) <- assigns, u == uv' ]
  next :: [MUV] -> [MUV]
  next uvs = sort $ nub $ uvs ++ concatMap previousUVs uvs

fixedpoint :: Eq a => (a -> a) -> a -> a
fixedpoint f a | a == f a  = a
               | otherwise = fixedpoint f $ f a

-- | All primary expressions used in a rule.
allUEs :: Rule -> [Hash]
allUEs rule = ruleEnable rule : ues
  where
  index :: MUV -> [Hash]
  index (MUVArray _ ue') = [ue']
  index _ = []
  ues = case rule of
    Rule _ _ _ _ _ _ _ -> 
         concat [ ue' : index uv' | (uv', ue') <- ruleAssigns rule ] 
      ++ concat (snd (unzip (ruleActions rule)))
    Assert _ _ a       -> [a]
    Cover  _ _ a       -> [a]

