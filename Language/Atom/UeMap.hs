-- | 
-- Module: UeMap
-- Description: Sharing for UEs, based on IntMaps.
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Sharing for 'UE's, based on IntMaps.  The idea is to share subexpressions
-- of 'UE's.

module Language.Atom.UeMap
  ( UeElem (..)
  , MUV (..)
  , UeMap
  , emptyMap
  , Hash
  , typeOf
  , UeState
  , recoverUE
  , getUE
  , newUE
  , newUV
  , maybeUpdate
  , ueUpstream
  , nearestUVs
  , arrayIndices
  , isMathHCall
  ) where

import Control.Monad.State.Strict
import qualified Data.Bimap as M
import Data.List (nub)

import Language.Atom.Expressions hiding (typeOf)
import qualified Language.Atom.Expressions as E

type Hash = Int

-- | Untyped variables map.
data MUV
  = MUV Int String Const
  | MUVArray UA Hash
  | MUVExtern String Type
  deriving (Show, Eq, Ord)

-- | Transforms a 'UV' into a 'MUV', returning the possibly updated map.
newUV :: UV -> UeMap -> (MUV, UeMap)
newUV u mp =
  case u of
    UV i j k        -> (MUV i j k, mp)
    UVExtern i j    -> (MUVExtern i j, mp)
    UVArray arr ue_ -> let (h,mp') = newUE ue_ mp in
                       (MUVArray arr h, mp')

-- | Corresponds to 'UE's --- the elements in the sharing structure.
data UeElem
  = MUVRef     !MUV
  | MUConst    !Const
  | MUCast     !Type !Hash
  | MUAdd      !Hash !Hash
  | MUSub      !Hash !Hash
  | MUMul      !Hash !Hash
  | MUDiv      !Hash !Hash
  | MUMod      !Hash !Hash
  | MUNot      !Hash
  | MUAnd      [Hash]
  | MUBWNot    !Hash
  | MUBWAnd    !Hash !Hash
  | MUBWOr     !Hash !Hash
  | MUBWXor    !Hash !Hash
  | MUBWShiftL !Hash !Hash
  | MUBWShiftR !Hash !Hash
  | MUEq       !Hash !Hash
  | MULt       !Hash !Hash
  | MUMux      !Hash !Hash !Hash
  | MUF2B      !Hash
  | MUD2B      !Hash
  | MUB2F      !Hash
  | MUB2D      !Hash
-- math.h:
  | MUPi
  | MUExp      !Hash
  | MULog      !Hash
  | MUSqrt     !Hash
  | MUPow      !Hash !Hash
  | MUSin      !Hash
  | MUAsin     !Hash
  | MUCos      !Hash
  | MUAcos     !Hash
  | MUSinh     !Hash
  | MUCosh     !Hash
  | MUAsinh    !Hash
  | MUAcosh    !Hash
  | MUAtan     !Hash
  | MUAtanh    !Hash
  deriving (Show, Eq, Ord)

typeOf :: Hash -> UeMap -> Type
typeOf h mp = case getUE h mp of
    MUVRef     (MUV _ _ a)     -> E.typeOf a
    MUVRef     (MUVArray a _)  -> E.typeOf a
    MUVRef     (MUVExtern _ t) -> t
    MUCast     t _             -> t
    MUConst    c               -> E.typeOf c
    MUAdd      a _             -> typeOf' a
    MUSub      a _             -> typeOf' a
    MUMul      a _             -> typeOf' a
    MUDiv      a _             -> typeOf' a
    MUMod      a _             -> typeOf' a
    MUNot      _               -> Bool
    MUAnd      _               -> Bool
    MUBWNot    a               -> typeOf' a
    MUBWAnd    a _             -> typeOf' a
    MUBWOr     a _             -> typeOf' a
    MUBWXor    a _             -> typeOf' a
    MUBWShiftL a _             -> typeOf' a
    MUBWShiftR a _             -> typeOf' a
    MUEq       _ _             -> Bool
    MULt       _ _             -> Bool
    MUMux      _ a _           -> typeOf' a
    MUF2B      _               -> Word32
    MUD2B      _               -> Word64
    MUB2F      _               -> Float
    MUB2D      _               -> Double
-- math.h:
    MUPi                       -> Double
    MUExp     a                -> typeOf' a
    MULog     a                -> typeOf' a
    MUSqrt    a                -> typeOf' a
    MUPow     a _              -> typeOf' a
    MUSin     a                -> typeOf' a
    MUAsin    a                -> typeOf' a
    MUCos     a                -> typeOf' a
    MUAcos    a                -> typeOf' a
    MUSinh    a                -> typeOf' a
    MUCosh    a                -> typeOf' a
    MUAsinh   a                -> typeOf' a
    MUAcosh   a                -> typeOf' a
    MUAtan    a                -> typeOf' a
    MUAtanh   a                -> typeOf' a
  where
  typeOf' h' = typeOf h' mp

-- | An entry in the Map.
type UeMap = (Hash, M.Bimap Int UeElem)

-- | Wrapped in the State Monad.
type UeState a = State UeMap a

-- | Get the element associated with a 'Hash' value.  It's an error if the
-- element is not in the map.
getUE :: Hash -> UeMap -> UeElem
getUE h (_,mp) =
  case M.lookup h mp of
    Nothing -> error $ "Error looking up hash " ++ show h ++ " in the UE map\n" ++ show mp
    Just e -> e

-- | Put a new 'UE' in the map, unless it's already in there, and return the
-- hash pointing to the 'UE' and a new map.
newUE :: UE -> UeMap -> (Hash, UeMap)
newUE ue_ mp = runState (share ue_) mp

emptyMap :: UeMap
emptyMap = (0, M.empty)

-- | Create the sharing map.
share :: UE -> UeState Hash
share e = case e of
  UVRef     (UV i j k)      -> maybeUpdate (MUVRef $ MUV i j k)
  UVRef     (UVExtern i j)  -> maybeUpdate (MUVRef $ MUVExtern i j)
  UVRef     (UVArray arr a) -> unOp a (\x -> MUVRef (MUVArray arr x))
  UConst    a     -> maybeUpdate (MUConst a)
  UCast     t a   -> unOp a (MUCast t)
  UAdd      a b   -> binOp (a,b) MUAdd
  USub      a b   -> binOp (a,b) MUSub
  UMul      a b   -> binOp (a,b) MUMul
  UDiv      a b   -> binOp (a,b) MUDiv
  UMod      a b   -> binOp (a,b) MUMod
  UNot      a     -> unOp a MUNot
  UAnd      ls    -> listOp ls MUAnd
  UBWNot    a     -> unOp a MUBWNot
  UBWAnd    a b   -> binOp (a,b) MUBWAnd
  UBWOr     a b   -> binOp (a,b) MUBWOr
  UBWXor    a b   -> binOp (a,b) MUBWXor
  UBWShiftL a b   -> binOp (a,b) MUBWShiftL
  UBWShiftR a b   -> binOp (a,b) MUBWShiftR
  UEq       a b   -> binOp (a,b) MUEq
  ULt       a b   -> binOp (a,b) MULt
  UMux      a b c -> triOp (a,b,c) MUMux
  UF2B      a     -> unOp a MUF2B
  UD2B      a     -> unOp a MUD2B
  UB2F      a     -> unOp a MUB2F
  UB2D      a     -> unOp a MUB2D
-- math.h:
  UPi             -> maybeUpdate (MUPi)
  UExp      a     -> unOp a MUExp
  ULog      a     -> unOp a MULog
  USqrt     a     -> unOp a MUSqrt
  UPow      a b   -> binOp (a,b) MUPow
  USin      a     -> unOp a MUSin
  UAsin     a     -> unOp a MUAsin
  UCos      a     -> unOp a MUCos
  UAcos     a     -> unOp a MUAcos
  USinh     a     -> unOp a MUSinh
  UCosh     a     -> unOp a MUCosh
  UAsinh    a     -> unOp a MUAsinh
  UAcosh    a     -> unOp a MUAcosh
  UAtan     a     -> unOp a MUAtan
  UAtanh    a     -> unOp a MUAtanh

-- XXX I could combine some of the following functions (unOp, binOp, etc.) to
-- slightly reduce code...
unOp :: UE -> (Hash -> UeElem) -> UeState Hash
unOp e code = do
  h <- share e
  maybeUpdate (code h)

binOp :: (UE, UE) -> (Hash -> Hash -> UeElem) -> UeState Hash
binOp (e0,e1) code = do
  h0 <- share e0
  h1 <- share e1
  maybeUpdate (code h0 h1)

triOp :: (UE, UE, UE) -> (Hash -> Hash -> Hash -> UeElem) -> UeState Hash
triOp (e0,e1,e2) code = do
  h0 <- share e0
  h1 <- share e1
  h2 <- share e2
  maybeUpdate (code h0 h1 h2)

listOp :: [UE] -> ([Hash] -> UeElem) -> UeState Hash
listOp es code = do
  hashes <- foldM (\hashes e -> do h <- share e
                                   return (h:hashes)
                  ) [] es
  maybeUpdate (code hashes)

-- | Lookup an element in the map, and if it's in there, do nothing, but return
-- its hash value.  Otherwise, update the map and return the new hash value
-- for the inserted element.
maybeUpdate :: UeElem -> UeState Hash
maybeUpdate e = do
  st <- get
  let mp = snd st
  case M.lookupR e mp of
    Nothing -> do let hash = fst st + 1
                  put (hash, M.insert hash e mp)
                  return hash
    Just h -> return h

-- -- Lookup an elem, returning 'Nothing' if no hash exists in the map and 'Just'
-- -- the hash value otherwise.
-- getHash :: UeElem -> UeMap -> Maybe Hash
-- getHash e mp = M.lookupR e


-- ((k,e'):_) | e == e' = Just k
-- getHash e (_:es) | otherwise = getHash e es
-- getHash _ [] = Nothing

-- | Get a 'UE' back out of the 'UeMap'.
recoverUE :: UeMap -> Hash -> UE
recoverUE st h = case getUE h st of
  MUVRef     (MUV i j k)     -> UVRef (UV i j k)
  MUVRef     (MUVArray i a)  -> UVRef (UVArray i (recover' a))
  MUVRef     (MUVExtern i j) -> UVRef (UVExtern i j)
  MUCast     t a   -> UCast     t (recover' a)
  MUConst    a     -> UConst    a
  MUAdd      a b   -> UAdd      (recover' a) (recover' b)
  MUSub      a b   -> USub      (recover' a) (recover' b)
  MUMul      a b   -> UMul      (recover' a) (recover' b)
  MUDiv      a b   -> UDiv      (recover' a) (recover' b)
  MUMod      a b   -> UMod      (recover' a) (recover' b)
  MUNot      a     -> UNot      (recover' a)
  MUAnd      a     -> UAnd $ map recover' a
  MUBWNot    a     -> UBWNot    (recover' a)
  MUBWAnd    a b   -> UBWAnd    (recover' a) (recover' b)
  MUBWOr     a b   -> UBWOr     (recover' a) (recover' b)
  MUBWXor    a b   -> UBWXor    (recover' a) (recover' b)
  MUBWShiftL a b   -> UBWShiftL (recover' a) (recover' b)
  MUBWShiftR a b   -> UBWShiftR (recover' a) (recover' b)
  MUEq       a b   -> UEq       (recover' a) (recover' b)
  MULt       a b   -> ULt       (recover' a) (recover' b)
  MUMux      a b c -> UMux      (recover' a) (recover' b) (recover' c)
  MUF2B      a     -> UF2B      (recover' a)
  MUD2B      a     -> UD2B      (recover' a)
  MUB2F      a     -> UB2F      (recover' a)
  MUB2D      a     -> UB2D      (recover' a)
-- math.h:
  MUPi             -> UPi
  MUExp      a     -> UExp      (recover' a)
  MULog      a     -> ULog      (recover' a)
  MUSqrt     a     -> USqrt     (recover' a)
  MUPow      a b   -> UPow      (recover' a) (recover' b)
  MUSin      a     -> USin      (recover' a)
  MUAsin     a     -> UAsin     (recover' a)
  MUCos      a     -> UCos      (recover' a)
  MUAcos     a     -> UAcos     (recover' a)
  MUSinh     a     -> USinh     (recover' a)
  MUCosh     a     -> UCosh     (recover' a)
  MUAsinh    a     -> UAsinh    (recover' a)
  MUAcosh    a     -> UAcosh    (recover' a)
  MUAtan     a     -> UAtan     (recover' a)
  MUAtanh    a     -> UAtanh    (recover' a)
  where recover' h' = recoverUE st h'

-- | The list of Hashes to adjacent upstream of a UE.
ueUpstream :: Hash -> UeMap -> [Hash]
ueUpstream h t = case getUE h t of
  MUVRef     (MUV _ _ _)     -> []
  MUVRef     (MUVArray _ a)  -> [a]
  MUVRef     (MUVExtern _ _) -> []
  MUCast     _ a             -> [a]
  MUConst    _               -> []
  MUAdd      a b             -> [a, b]
  MUSub      a b             -> [a, b]
  MUMul      a b             -> [a, b]
  MUDiv      a b             -> [a, b]
  MUMod      a b             -> [a, b]
  MUNot      a               -> [a]
  MUAnd      a               -> a
  MUBWNot    a               -> [a]
  MUBWAnd    a b             -> [a, b]
  MUBWOr     a b             -> [a, b]
  MUBWXor    a b             -> [a, b]
  MUBWShiftL a b             -> [a, b]
  MUBWShiftR a b             -> [a, b]
  MUEq       a b             -> [a, b]
  MULt       a b             -> [a, b]
  MUMux      a b c           -> [a, b, c]
  MUF2B      a               -> [a]
  MUD2B      a               -> [a]
  MUB2F      a               -> [a]
  MUB2D      a               -> [a]
-- math.h:
  MUPi                       -> []
  MUExp      a               -> [a]
  MULog      a               -> [a]
  MUSqrt     a               -> [a]
  MUPow      a b             -> [a, b]
  MUSin      a               -> [a]
  MUAsin     a               -> [a]
  MUCos      a               -> [a]
  MUAcos     a               -> [a]
  MUSinh     a               -> [a]
  MUCosh     a               -> [a]
  MUAsinh    a               -> [a]
  MUAcosh    a               -> [a]
  MUAtan     a               -> [a]
  MUAtanh    a               -> [a]

-- | The list of all UVs that directly control the value of an expression.
nearestUVs :: Hash -> UeMap -> [MUV]
nearestUVs h mp = nub $ f h
  where
  f :: Hash -> [MUV]
  f hash = case getUE hash mp of
             (MUVRef u@(MUVArray _ h')) -> [u] ++ f h'
             (MUVRef u)                 -> [u]
             _                          -> concatMap f $ ueUpstream hash mp

-- | All array indexing subexpressions.
arrayIndices :: Hash -> UeMap -> [(UA, Hash)]
arrayIndices h mp = nub $ f h
  where
  f :: Hash -> [(UA, Hash)]
  f hash = case getUE hash mp of
             (MUVRef (MUVArray ua h')) -> (ua, h') : f h'
             _ -> concatMap f $ ueUpstream hash mp

-- XXX can put this back after making UE map---won't be expensive.
isMathHCall :: UeElem -> Bool
isMathHCall fc =
  case fc of
    MUPi        -> True
    MUExp   _   -> True
    MULog   _   -> True
    MUSqrt  _   -> True
    MUPow   _ _ -> True
    MUSin   _   -> True
    MUAsin  _   -> True
    MUCos   _   -> True
    MUAcos  _   -> True
    MUSinh  _   -> True
    MUCosh  _   -> True
    MUAsinh _   -> True
    MUAcosh _   -> True
    MUAtan  _   -> True
    MUAtanh _   -> True
    _          -> False

