module Language.Atom.Elaboration
  (
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

import Control.Monad.Trans
import Data.Function (on)
import Data.List
import Data.Char
import Language.Atom.Expressions

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
  , gProbes  :: [(String, UE)]
  , gPeriod  :: Int
  , gPhase   :: Phase
  }

data AtomDB = AtomDB
  { atomId          :: Int
  , atomName        :: Name
  , atomNames       :: [Name]      -- Names used at this level.
  , atomEnable      :: UE          -- Enabling condition.
  , atomSubs        :: [AtomDB]    -- Sub atoms.
  , atomPeriod      :: Int
  , atomPhase       :: Phase
  , atomAssigns     :: [(UV, UE)]
  , atomActions     :: [([String] -> String, [UE])]
  , atomAsserts     :: [(Name, UE)]
  , atomCovers      :: [(Name, UE)]
  }

data Rule
  = Rule
    { ruleId        :: Int
    , ruleName      :: Name
    , ruleEnable    :: UE
    , ruleAssigns   :: [(UV, UE)]
    , ruleActions   :: [([String] -> String, [UE])]
    , rulePeriod    :: Int
    , rulePhase     :: Phase
    , mathH         :: Bool -- Contains a math.h call?
    }
  | Assert
    { ruleName      :: Name
    , ruleEnable    :: UE
    , ruleAssert    :: UE
    }
  | Cover
    { ruleName      :: Name
    , ruleEnable    :: UE
    , ruleCover     :: UE
    }

data StateHierarchy
  = StateHierarchy Name [StateHierarchy]
  | StateVariable  Name Const
  | StateArray     Name [Const]

instance Show AtomDB where show = atomName
instance Eq   AtomDB where (==) = (==) `on` atomId
instance Ord  AtomDB where compare a b = compare (atomId a) (atomId b)
instance Show Rule   where show = ruleName

elaborateRules:: UE -> AtomDB -> [Rule]
elaborateRules parentEnable atom = if isRule then rule : rules else rules
  where
  isRule = not $ null (atomAssigns atom) && null (atomActions atom)
  enable = uand parentEnable $ atomEnable atom
  rule = Rule
    { ruleId        = atomId   atom
    , ruleName      = atomName atom
    , ruleEnable    = enable
    , ruleAssigns   = map enableAssign $ atomAssigns atom
    , ruleActions   = atomActions atom
    , rulePeriod    = atomPeriod  atom
    , rulePhase     = atomPhase   atom
    , mathH         = any isMathHCall (allUEs rule)
    }
  assert (name, ue) = Assert
    { ruleName      = name
    , ruleEnable    = enable
    , ruleAssert    = ue
    }
  cover (name, ue) = Cover
    { ruleName      = name
    , ruleEnable    = enable
    , ruleCover     = ue
    }
  rules =    map assert (atomAsserts atom) 
          ++ map cover (atomCovers atom) 
          ++ concatMap (elaborateRules enable) (atomSubs atom)
  enableAssign :: (UV, UE) -> (UV, UE)
  enableAssign (uv, ue) = (uv, umux enable ue $ UVRef uv)

reIdRules :: Int -> [Rule] -> [Rule]
reIdRules _ [] = []
reIdRules i (a:b) = case a of
  Rule _ _ _ _ _ _ _ _ -> a { ruleId = i } : reIdRules (i + 1) b
  _                    -> a                : reIdRules  i      b

buildAtom :: Global -> Name -> Atom a -> IO (a, (Global, AtomDB))
buildAtom g name (Atom f) = f (g { gRuleId = gRuleId g + 1 }, AtomDB
  { atomId        = gRuleId g
  , atomName      = name
  , atomNames     = []
  , atomEnable    = ubool True
  , atomSubs      = []
  , atomPeriod    = gPeriod g
  , atomPhase     = gPhase  g
  , atomAssigns   = []
  , atomActions   = []
  , atomAsserts   = []
  , atomCovers    = []
  })

-- | The Atom monad holds variable and rule declarations.
data Atom a = Atom ((Global, AtomDB) -> IO (a, (Global, AtomDB)))

instance Monad Atom where
  return a = Atom (\ s -> return (a, s))
  (Atom f1) >>= f2 = Atom f3
    where
    f3 s = do
      (a, s) <- f1 s
      let Atom f4 = f2 a
      f4 s

instance MonadIO Atom where
  liftIO io = Atom f
    where
    f s = do
      a <- io
      return (a, s)

get :: Atom (Global, AtomDB)
get = Atom (\ s -> return (s, s))

put :: (Global, AtomDB) -> Atom ()
put s = Atom (\ _ -> return ((), s))

-- | A Relation is used for relative performance constraints between 'Action's.
-- data Relation = Higher UID | Lower UID deriving (Show, Eq)

-- | Given a top level name and design, elaborates design and returns a design database.
elaborate :: Name -> Atom () -> IO (Maybe (StateHierarchy, [Rule], [Name], [Name], [(Name, Type)]))
elaborate name atom = do
  (_, (g, atomDB)) <- buildAtom Global { gRuleId = 0
                                       , gVarId = 0
                                       , gArrayId = 0
                                       , gState = []
                                       , gProbes = []
                                       , gPeriod = 1
                                       , gPhase  = MinPhase 0 
                                       } name atom
  let rules = reIdRules 0 $ elaborateRules (ubool True) atomDB
      coverageNames  = [ name | Cover  name _ _ <- rules ]
      assertionNames = [ name | Assert name _ _ <- rules ]
      probeNames = [ (n, typeOf a) | (n, a) <- gProbes g ]
  if (null rules)
    then do
      putStrLn "ERROR: Design contains no rules.  Nothing to do."
      return Nothing
    else do
      mapM_ checkEnable rules
      ok <- mapM checkAssignConflicts rules
      return (if and ok 
                then Just (trimState $ StateHierarchy name 
                            $ gState g, rules, assertionNames, coverageNames, probeNames) 
                else Nothing)

trimState :: StateHierarchy -> StateHierarchy
trimState a = case a of
  StateHierarchy name items -> StateHierarchy name $ filter f $ map trimState items
  a -> a
  where
  f (StateHierarchy _ []) = False
  f _ = True


-- | Checks that a rule will not be trivially disabled.
checkEnable :: Rule -> IO ()
checkEnable rule | ruleEnable rule == ubool False = putStrLn $ "WARNING: Rule will never execute: " ++ show rule
                 | otherwise                      = return ()

-- | Check that a variable is assigned more than once in a rule.  Will eventually be replaced consistent assignment checking.
checkAssignConflicts :: Rule -> IO Bool
checkAssignConflicts rule@(Rule _ _ _ _ _ _ _ _) =
  if length vars /= length vars'
    then do
      putStrLn $ "ERROR: Rule " ++ show rule ++ " contains multiple assignments to the same variable(s)."
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
var name init = do
  name' <- addName name
  (g, atom) <- get
  let uv = UV (gVarId g) name' c
      c = constant init
  put (g { gVarId = gVarId g + 1, gState = gState g ++ [StateVariable name c] }, atom)
  return $ V uv

-- | Generic external variable declaration.
var' :: Name -> Type -> V a
var' name t = V $ UVExtern name t

-- | Generic array declaration.
array :: Expr a => Name -> [a] -> Atom (A a)
array name [] = error $ "ERROR: arrays can not be empty: " ++ name
array name init = do
  name' <- addName name
  (g, atom) <- get
  let ua = UA (gArrayId g) name' c
      c = map constant init
  put (g { gArrayId = gArrayId g + 1, gState = gState g ++ [StateArray name c] }, atom)
  return $ A ua

-- | Generic external array declaration.
array' :: Expr a => Name -> Type -> A a
array' name t = A $ UAExtern  name t

addName :: Name -> Atom Name
addName name = do
  (g, atom) <- get
  checkName name
  if elem name (atomNames atom)
    then error $ "ERROR: Name \"" ++ name ++ "\" not unique in " ++ show atom ++ "."
    else do
      put (g, atom { atomNames = name : atomNames atom })
      return $ atomName atom ++ "." ++ name

-- still accepts some misformed names
checkName :: Name -> Atom ()
checkName name =
  if (\ x -> isAlpha x || x == '_') (head name) && 
      and (map (\ x -> isAlphaNum x || x `elem` "._-[]") (tail name)) && 
      and (map isAscii name)
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
allUVs :: [Rule] -> UE -> [UV]
allUVs rules ue = fixedpoint next $ nearestUVs ue
  where
  assigns = concat [ ruleAssigns r | r@(Rule _ _ _ _ _ _ _ _) <- rules ]
  previousUVs :: UV -> [UV]
  previousUVs uv = concat [ nearestUVs ue | (uv', ue) <- assigns, uv == uv' ]
  next :: [UV] -> [UV]
  next uvs = sort $ nub $ uvs ++ concatMap previousUVs uvs

fixedpoint :: Eq a => (a -> a) -> a -> a
fixedpoint f a | a == f a  = a
               | otherwise = fixedpoint f $ f a

-- | All primary expressions used in a rule.
allUEs :: Rule -> [UE]
allUEs rule = ruleEnable rule : ues
  where
  index :: UV -> [UE]
  index (UVArray _ ue) = [ue]
  index _ = []
  ues = case rule of
    Rule _ _ _ _ _ _ _ _ -> 
         concat [ ue : index uv | (uv, ue) <- ruleAssigns rule ] 
      ++ concat (snd (unzip (ruleActions rule)))
    Assert _ _ a       -> [a]
    Cover  _ _ a       -> [a]

