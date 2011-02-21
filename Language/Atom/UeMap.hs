-- | Sharing for UEs, based on IntMaps.  The idea is to share subexpressions of 'UE's.

module UeMap ( ) where

import Control.Monad.State
import qualified Data.IntMap as M
import Data.Maybe

import Language.Atom.Expressions 
type Hash = Int

-- | Corresponds to 'UE's --- the elements in the sharing structure.
data UeElem
  = MUVRef UV
  | MUConst Const
  | MUCast  Type Hash
  | MUAdd   Hash Hash
  | MUSub   Hash Hash
  | MUMul   Hash Hash
  | MUDiv   Hash Hash
  | MUMod   Hash Hash
  | MUNot   Hash
  | MUAnd   [Hash]
  | MUBWNot Hash
  | MUBWAnd Hash Hash
  | MUBWOr  Hash Hash
  | MUShift Hash Int
  | MUEq    Hash Hash
  | MULt    Hash Hash
  | MUMux   Hash Hash Hash
  | MUF2B   Hash
  | MUD2B   Hash
  | MUB2F   Hash
  | MUB2D   Hash
-- math.h:
  | MUPi
  | MUExp   Hash
  | MULog   Hash
  | MUSqrt  Hash
  | MUPow   Hash Hash
  | MUSin   Hash
  | MUAsin  Hash
  | MUCos   Hash
  | MUAcos  Hash
  | MUSinh  Hash
  | MUCosh  Hash
  | MUAsinh Hash
  | MUAcosh Hash
  | MUAtan  Hash
  | MUAtanh Hash
  deriving (Show, Eq, Ord)

-- | An entry in the Map.
type UeEntry = (Hash, M.IntMap UeElem)

-- | Wrapped in the State Monad.
type UeMap a = State UeEntry a

-- | Create the sharing map.
share :: UE -> UeMap Hash
share e = 
  case e of 
  UVRef v   -> maybeUpdate (MUVRef v)
  UConst  a     -> maybeUpdate (MUConst a)
  UCast   t a   -> unOp a (MUCast t)
  UAdd    a b   -> binOp (a,b) MUAdd
  USub    a b   -> binOp (a,b) MUSub
  UMul    a b   -> binOp (a,b) MUMul
  UDiv    a b   -> binOp (a,b) MUDiv
  UMod    a b   -> binOp (a,b) MUMod
  UNot    a     -> unOp a MUNot
  UAnd    ls    -> listOp ls MUAnd
  UBWNot  a     -> unOp a MUBWNot
  UBWAnd  a b   -> binOp (a,b) MUBWAnd
  UBWOr   a b   -> binOp (a,b) MUBWOr
  UShift  a b   -> unOp a (\x -> MUShift x b)
  UEq     a b   -> binOp (a,b) MUEq
  ULt     a b   -> binOp (a,b) MULt
  UMux    a b c -> triOp (a,b,c) MUMux
  UF2B    a     -> unOp a MUF2B
  UD2B    a     -> unOp a MUD2B
  UB2F    a     -> unOp a MUB2F
  UB2D    a     -> unOp a MUB2D
-- math.h:
  UPi           -> maybeUpdate (MUPi)
  UExp    a     -> unOp a MUExp
  ULog    a     -> unOp a MULog
  USqrt   a     -> unOp a MUSqrt
  UPow    a b   -> binOp (a,b) MUPow
  USin    a     -> unOp a MUSin
  UAsin   a     -> unOp a MUAsin
  UCos    a     -> unOp a MUCos
  UAcos   a     -> unOp a MUAcos
  USinh   a     -> unOp a MUSinh
  UCosh   a     -> unOp a MUCosh
  UAsinh  a     -> unOp a MUAsinh
  UAcosh  a     -> unOp a MUAcosh
  UAtan   a     -> unOp a MUAtan
  UAtanh  a     -> unOp a MUAtanh

-- XXX I could combine some of the following functions (unOp, binOp, etc.) to
-- slightly reduce code...
unOp :: UE -> (Hash -> UeElem) -> UeMap Hash
unOp e code = do
  h <- share e  
  maybeUpdate (code h)

binOp :: (UE, UE) -> (Hash -> Hash -> UeElem) -> UeMap Hash
binOp (e0,e1) code = do
  h0 <- share e0  
  h1 <- share e1  
  maybeUpdate (code h0 h1)

triOp :: (UE, UE, UE) -> (Hash -> Hash -> Hash -> UeElem) -> UeMap Hash
triOp (e0,e1,e2) code = do
  h0 <- share e0  
  h1 <- share e1  
  h2 <- share e2  
  maybeUpdate (code h0 h1 h2)

listOp :: [UE] -> ([Hash] -> UeElem) -> UeMap Hash
listOp es code = do
  hashes <- foldM (\hashes e -> do h <- share e 
                                   return (h:hashes)
                  ) [] es
  maybeUpdate (code hashes)

-- | Lookup an element in the map, and if it's in there, do nothing, but return
-- its hash value.  Otherwise, update the map and return the new hash value
-- for the inserted element.
maybeUpdate :: UeElem -> UeMap Hash
maybeUpdate code = do
  st <- get
  case getHash code (snd st) of
    Nothing -> update code st
    Just h -> return h
  where
  -- Update the map.
  update :: UeElem -> UeEntry -> UeMap Hash
  update code st = do let hash = fst st + 1
                      put (hash, M.insert hash code (snd st))
                      return hash
  -- Lookup a hash value, returning 'Nothing' if no hash exists in the map and
  -- 'Just' the hash value otherwise.
  getHash :: UeElem -> M.IntMap UeElem -> Maybe Hash
  getHash e st = 
    M.foldWithKey (\k code m -> if isJust m then m
                                  else if e == code then Just k
                                         else Nothing) Nothing st
