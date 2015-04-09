-- | 
-- Module: Analysis
-- Description: -
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike

module Language.Atom.Analysis
  ( topo
  , ruleComplexity
  ) where

import Language.Atom.Elaboration
import Language.Atom.UeMap

-- | Topologically sorts a list of expressions and subexpressions.
topo :: UeMap -> [Hash] -> [(Hash, String)]
topo mp ues = reverse ues'
  where
  start = 0
  (_, ues') = foldl collect (start, []) ues
  collect :: (Int, [(Hash, String)]) -> Hash -> (Int, [(Hash, String)])
  collect (n, ues_) ue | any ((== ue) . fst) ues_ = (n, ues_)
  collect (n, ues_) ue = (n' + 1, (ue, e n') : ues'') 
    where (n', ues'') = foldl collect (n, ues_) $ ueUpstream ue mp

e :: Int -> String
e i = "__" ++ show i

-- | Number of UE's computed in rule.
ruleComplexity :: UeMap -> Rule -> Int
ruleComplexity mp = length . (topo mp) . allUEs

