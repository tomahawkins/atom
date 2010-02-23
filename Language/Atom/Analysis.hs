module Language.Atom.Analysis
  ( topo
  , ruleComplexity
  ) where

import Language.Atom.Elaboration
import Language.Atom.Expressions

-- | Topologically sorts a list of expressions and subexpressions.
topo :: [UE] -> [(UE, String)]
topo ues = reverse ues'
  where
  start = 0
  (_, ues') = foldl collect (start, []) ues
  collect :: (Int, [(UE, String)]) -> UE -> (Int, [(UE, String)])
  collect (n, ues) ue | any ((== ue) . fst) ues = (n, ues)
  collect (n, ues) ue = (n' + 1, (ue, e n') : ues') where (n', ues') = foldl collect (n, ues) $ ueUpstream ue

e :: Int -> String
e i = "__" ++ show i

-- | Number of UE's computed in rule.
ruleComplexity :: Rule -> Int
ruleComplexity = length . topo . allUEs

