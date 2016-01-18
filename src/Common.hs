module Common
  ( Name
  , Path
  , split
  ) where

type Name = String
type Path = [Name]

split :: Eq a => a -> [a] -> [[a]]
split a b
  | null b1 = [b0]
  | otherwise = b0 : split a (tail b1)
  where
  (b0, b1) = break (a ==) b

