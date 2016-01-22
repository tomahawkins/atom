module Common
  ( Name
  , Path
  , split
  , Location (..)
  , Locate   (..)
  ) where

type Name = String
type Path = [Name]

split :: Eq a => a -> [a] -> [[a]]
split a b
  | null b1 = [b0]
  | otherwise = b0 : split a (tail b1)
  where
  (b0, b1) = break (a ==) b

data Location = Location String Int Int deriving Eq

instance Show Location where
  show (Location f l c) = f ++ ":" ++ show l ++ ":" ++ show c
   
class    Locate a        where locate :: a -> Location
instance Locate Location where locate = id


