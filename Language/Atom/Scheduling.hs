-- | 
-- Module: Scheduling
-- Description: Rule scheduling
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Algorithms for scheduling rules in Atom

module Language.Atom.Scheduling
  ( schedule
  , Schedule
  , reportSchedule
  ) where

import Text.Printf
import Data.List

import Language.Atom.Analysis
import Language.Atom.Elaboration
import Language.Atom.UeMap

-- | Schedule expressed as a 'UeMap' and a list of (period, phase, rules).
type Schedule = (UeMap, [(Int, Int, [Rule])])

schedule :: [Rule] -> UeMap -> Schedule
schedule rules' mp = (mp, concatMap spread periods)
  where
  rules = [ r | r@(Rule _ _ _ _ _ _ _) <- rules' ]

  -- Algorithm for assigning rules to phases for a given period 
  -- (assuming they aren't given an exact phase):

    -- 1. List the rules by their offsets, highest first.

    -- 2. If the list is empty, stop.

    -- 3. Otherwise, take the head of the list and assign its phase as follows:
    -- find the set of phases containing the minimum number of rules such that
    -- they are at least as large as the rule's offset.  Then take the smallest
    -- of those phases.

    -- 4. Go to (2).

  -- Algorithm properties: for each period, 

    -- A. Each rule is scheduled no earlier than its offset.

    -- B. The phase with the most rules is the minimum of all possible schedules
    -- that satisfy (A).

    -- XXX Check if this is true.
    -- C. The sum of the difference between between each rule's offset and it's
    -- scheduled phase is the minimum of all schedules satisfying (A) and (B).
    
  spread :: (Int, [Rule]) -> [(Int, Int, [Rule])]
  spread (period, rules_) = 
    placeRules (placeExactRules (replicate period []) exactRules)
               orderedByPhase
    where
    (minRules,exactRules) = partition (\r -> case rulePhase r of
                                               MinPhase _   -> True
                                               ExactPhase _ -> False) rules_
    placeExactRules :: [[Rule]] -> [Rule] -> [[Rule]]
    placeExactRules ls [] = ls
    placeExactRules ls (r:rst) = placeExactRules (insertAt (getPh r) r ls)
                                 rst

    orderedByPhase :: [Rule]
    orderedByPhase = sortBy (\r0 r1 -> compare (getPh r1) (getPh r0)) minRules
    getPh r = case rulePhase r of
                MinPhase i   -> i
                ExactPhase i -> i

    -- Initially, ls contains all the exactPhase rules.  We put rules in those
    -- lists according to the algorithm, and then filter out the phase-lists
    -- with no rules.
    placeRules :: [[Rule]] -> [Rule] -> [(Int, Int, [Rule])]
    placeRules ls [] = filter (\(_,_,rls) -> not (null rls)) 
                              (zip3 (repeat period) [0..(period-1)] ls)
    placeRules ls (r:rst) = placeRules (insertAt (lub r ls) r ls) rst 

    lub :: Rule -> [[Rule]] -> Int
    lub r ls = let minI = getPh r
                   lub' i [] = i -- unreachable.  Included to prevent missing
                                 -- cases ghc warnings.
                   lub' i ls_ | (head ls_) == minimum ls_ = i
                              | otherwise = lub' (i+1) (tail ls_)
               in  lub' minI (drop minI $ map length ls)

    -- Cons rule r onto the list at index i in ls.
    insertAt :: Int -> Rule -> [[Rule]] -> [[Rule]]
    insertAt i r ls = (take i ls) ++ ((r:(ls !! i)):(drop (i+1) ls))

  periods = foldl grow [] [ (rulePeriod r, r) | r <- rules ]

  grow :: [(Int, [Rule])] -> (Int, Rule) -> [(Int, [Rule])]
  grow [] (a, b) = [(a, [b])]
  grow ((a, bs):rest) (a', b) | a' == a   = (a, b : bs) : rest
                              | otherwise = (a, bs) : grow rest (a', b)

-- | Generate a rule scheduling report for the given schedule.
reportSchedule :: Schedule -> String
reportSchedule (mp, schedule_) = concat
  [ "Rule Scheduling Report\n\n"
  , "Period  Phase  Exprs  Rule\n"
  , "------  -----  -----  ----\n"
  , concatMap (reportPeriod mp) schedule_
  , "               -----\n"
  , printf "               %5i\n" $ sum $ map (ruleComplexity mp) rules
  , "\n"
  , "Hierarchical Expression Count\n\n"
  , "  Total   Local     Rule\n"
  , "  ------  ------    ----\n"
  , reportUsage "" $ usage mp rules
  , "\n"
  ]
  where
  rules = concat $ [ r | (_, _, r) <- schedule_ ]

reportPeriod :: UeMap -> (Int, Int, [Rule]) -> String
reportPeriod mp (period, phase, rules) = concatMap reportRule rules
  where
  reportRule :: Rule -> String
  reportRule rule = printf "%6i  %5i  %5i  %s\n" period phase (ruleComplexity mp rule) (show rule)


data Usage = Usage String Int [Usage] deriving Eq

instance Ord Usage where compare (Usage a _ _) (Usage b _ _) = compare a b

reportUsage :: String -> Usage -> String
reportUsage i node@(Usage name n subs) = printf "  %6i  %6i    %s\n" (totalComplexity node) n (i ++ name) ++ concatMap (reportUsage ("  " ++ i)) subs

totalComplexity :: Usage -> Int
totalComplexity (Usage _ n subs) = n + sum (map totalComplexity subs)

usage :: UeMap -> [Rule] -> Usage
usage mp = head . foldl insertUsage [] . map (usage' mp)

usage' :: UeMap -> Rule -> Usage
usage' mp rule = f $ split $ ruleName rule
  where
  f :: [String] -> Usage
  f [] = undefined
  f [name] = Usage name (ruleComplexity mp rule) []
  f (name:names) = Usage name 0 [f names]

split :: String -> [String]
split "" = []
split s = a : if null b then [] else split (tail b) where (a,b) = span (/= '.') s

insertUsage :: [Usage] -> Usage -> [Usage]
insertUsage [] u = [u]
insertUsage (a@(Usage n1 i1 s1) : rest) b@(Usage n2 i2 s2) | n1 == n2  = Usage n1 (max i1 i2) (sort $ foldl insertUsage s1 s2) : rest
                                                           | otherwise = a : insertUsage rest b
