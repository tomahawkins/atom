-- | 
-- Module: Expressions
-- Description: Definitions for expressions, variables, and types
-- Copyright: (c) 2013 Tom Hawkins & Lee Pike
--
-- Definitions for Atom expressions, variables, and types
{-# LANGUAGE GADTs, DeriveDataTypeable #-}

module Language.Atom.Expressions
  (
  -- * Types
    E     (..)
  , V     (..)
  , UE    (..)
  , UV    (..)
  , A     (..)
  , UA    (..)
  , Expr  (..)
  , Expression (..)
  , Variable   (..)
  , Type  (..)
  , Const (..)
  , Width (..)
  , TypeOf (..)
  , bytes
  , ue
  , uv
--  , ueUpstream
--  , isMathHCall
--  , nearestUVs
--  , arrayIndices
  , NumE
  , IntegralE
  , FloatingE
  , EqE
  , OrdE
  -- * Constants
  , true
  , false
  -- * Variable Reference and Assignment
  , value
  -- * Logical Operations
  , not_
  , (&&.)
  , (||.)
  , and_
  , or_
  , any_
  , all_
  , imply
  -- * Bit-wise Operations
  , (.&.)
  , complement
  , (.|.)
  , xor
  , (.<<.)
  , (.>>.)
  , rol
  , ror
  , bitSize
  , isSigned
  -- * Equality and Comparison
  , (==.)
  , (/=.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  , min_
  , minimum_
  , max_
  , maximum_
  , limit
  -- * Arithmetic Operations
  , div_
  , div0_
  , mod_
  , mod0_
  -- * Conditional Operator
  , mux
  -- * Array Indexing
  , (!)
  , (!.)
  -- * Smart constructors for untyped expressions.
  , ubool
  , unot
  , uand
  , uor
  , ueq
  , umux
  ) where

import Data.Bits
import Data.Function (on)
import Data.Int
import Data.List
import Data.Ratio
import Data.Word

import Data.Generics hiding ( typeOf )

--infixl 7 /., %.
--infixl 6 +., -.
--infixr 5 ++.
infixl 9 !, !.
infix  4 ==., /=., <., <=., >., >=.
infixl 3 &&. --, ^. -- , &&&, $&, $&&
infixl 2 ||. -- , |||, $$, $:, $|
--infixr 1 -- <==, <-- -- , |->, |=>, -->

-- | The type of a 'E'.
data Type
  = Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | Word8
  | Word16
  | Word32
  | Word64
  | Float
  | Double
  deriving (Show, Read, Eq, Ord, Enum, Data, Typeable)

-- | Typed constant
data Const
  = CBool   Bool
  | CInt8   Int8
  | CInt16  Int16
  | CInt32  Int32
  | CInt64  Int64
  | CWord8  Word8
  | CWord16 Word16
  | CWord32 Word32
  | CWord64 Word64
  | CFloat  Float
  | CDouble Double
  deriving (Eq, Ord, Data, Typeable)

instance Show Const where
  show c' = case c' of
    CBool   True  -> "1"
    CBool   False -> "0"
    CInt8   c -> show c
    CInt16  c -> show c
    CInt32  c -> show c
    CInt64  c -> show c
    CWord8  c -> show c
    CWord16 c -> show c
    CWord32 c -> show c
    CWord64 c -> show c
    CFloat  c -> show c
    CDouble c -> show c

-- | Typed expression
data Expression
  = EBool   (E Bool)
  | EInt8   (E Int8)
  | EInt16  (E Int16)
  | EInt32  (E Int32)
  | EInt64  (E Int64)
  | EWord8  (E Word8)
  | EWord16 (E Word16)
  | EWord32 (E Word32)
  | EWord64 (E Word64)
  | EFloat  (E Float)
  | EDouble (E Double)

-- | Typed variable
data Variable
  = VBool   (V Bool)
  | VInt8   (V Int8)
  | VInt16  (V Int16)
  | VInt32  (V Int32)
  | VInt64  (V Int64)
  | VWord8  (V Word8)
  | VWord16 (V Word16)
  | VWord32 (V Word32)
  | VWord64 (V Word64)
  | VFloat  (V Float)
  | VDouble (V Double) deriving Eq

-- | Variables updated by state transition rules.
data V a = V UV deriving Eq

-- | Untyped variables.
data UV
  = UV Int String Const
  | UVArray UA UE
  | UVExtern String Type
  deriving (Show, Eq, Ord, Data, Typeable)

-- | A typed array.
data A a = A UA deriving Eq

-- | An untyped array.
data UA
  = UA Int String [Const]
  | UAExtern String Type
  deriving (Show, Eq, Ord, Data, Typeable)

-- | A typed expression.
data E a where
  VRef     :: V a -> E a
  Const    :: a -> E a
  Cast     :: (NumE a, NumE b) => E a -> E b
  Add      :: NumE a => E a -> E a -> E a
  Sub      :: NumE a => E a -> E a -> E a
  Mul      :: NumE a => E a -> E a -> E a
  Div      :: NumE a => E a -> E a -> E a
  Mod      :: IntegralE a => E a -> E a -> E a
  Not      :: E Bool -> E Bool
  And      :: E Bool -> E Bool -> E Bool
  BWNot    :: IntegralE a => E a -> E a
  BWAnd    :: IntegralE a => E a -> E a -> E a
  BWOr     :: IntegralE a => E a -> E a -> E a
  BWXor    :: IntegralE a => E a -> E a -> E a
  BWShiftL :: ( IntegralE a, IntegralE b ) => E a -> E b -> E a
  BWShiftR :: ( IntegralE a, IntegralE b ) => E a -> E b -> E a
  Eq       :: EqE a  => E a -> E a -> E Bool
  Lt       :: OrdE a => E a -> E a -> E Bool
  Mux      :: E Bool -> E a -> E a -> E a
  F2B      :: E Float  -> E Word32
  D2B      :: E Double -> E Word64
  B2F      :: E Word32 -> E Float
  B2D      :: E Word64 -> E Double
  Retype   :: UE -> E a
-- math.h:
  Pi       :: FloatingE a => E a
  Exp      :: FloatingE a => E a -> E a
  Log      :: FloatingE a => E a -> E a
  Sqrt     :: FloatingE a => E a -> E a
  Pow      :: FloatingE a => E a -> E a -> E a
  Sin      :: FloatingE a => E a -> E a
  Asin     :: FloatingE a => E a -> E a
  Cos      :: FloatingE a => E a -> E a
  Acos     :: FloatingE a => E a -> E a
  Sinh     :: FloatingE a => E a -> E a
  Cosh     :: FloatingE a => E a -> E a
  Asinh    :: FloatingE a => E a -> E a
  Acosh    :: FloatingE a => E a -> E a
  Atan     :: FloatingE a => E a -> E a
  Atanh    :: FloatingE a => E a -> E a

instance Show (E a) where
  show _ = error "Show (E a) not implemented"

instance Expr a => Eq (E a) where
  (==) = (==) `on` ue

-- | An untyped term.
data UE
  = UVRef     UV
  | UConst    Const
  | UCast     Type UE
  | UAdd      UE UE
  | USub      UE UE
  | UMul      UE UE
  | UDiv      UE UE
  | UMod      UE UE
  | UNot      UE
  | UAnd      [UE]
  | UBWNot    UE
  | UBWAnd    UE UE
  | UBWOr     UE UE
  | UBWXor    UE UE
  | UBWShiftL UE UE
  | UBWShiftR UE UE
  | UEq       UE UE
  | ULt       UE UE
  | UMux      UE UE UE
  | UF2B      UE
  | UD2B      UE
  | UB2F      UE
  | UB2D      UE
-- math.h:
  | UPi
  | UExp      UE
  | ULog      UE
  | USqrt     UE
  | UPow      UE UE
  | USin      UE
  | UAsin     UE
  | UCos      UE
  | UAcos     UE
  | USinh     UE
  | UCosh     UE
  | UAsinh    UE
  | UAcosh    UE
  | UAtan     UE
  | UAtanh    UE
  deriving (Show, Eq, Ord, Data, Typeable)

-- | Types with a defined width in bits
class Width a where
  -- | The width of a type, in number of bits
  width :: a -> Int

-- | The number of bytes that an object occupies
bytes :: Width a => a -> Int
bytes a = div (width a) 8 + if mod (width a) 8 == 0 then 0 else 1

instance Width Type where
  width t = case t of
    Bool   -> 1
    Int8   -> 8
    Int16  -> 16
    Int32  -> 32
    Int64  -> 64
    Word8  -> 8
    Word16 -> 16
    Word32 -> 32
    Word64 -> 64
    Float  -> 32
    Double -> 64

instance Width Const           where width = width . typeOf
instance Expr a => Width (E a) where width = width . typeOf
instance Expr a => Width (V a) where width = width . typeOf
instance Width UE              where width = width . typeOf
instance Width UV              where width = width . typeOf

-- | Types which have a defined 'Type'
class TypeOf a where
  -- | The corresponding 'Type' of the given object
  typeOf :: a -> Type

instance TypeOf Const where
  typeOf a = case a of
    CBool   _ -> Bool
    CInt8   _ -> Int8
    CInt16  _ -> Int16
    CInt32  _ -> Int32
    CInt64  _ -> Int64
    CWord8  _ -> Word8
    CWord16 _ -> Word16
    CWord32 _ -> Word32
    CWord64 _ -> Word64
    CFloat  _ -> Float
    CDouble _ -> Double

instance TypeOf UV where
  typeOf a' = case a' of
    UV _ _ a     -> typeOf a
    UVArray a _  -> typeOf a
    UVExtern _ t -> t

instance TypeOf (V a) where
  typeOf (V uv') = typeOf uv'

instance TypeOf UA where
  typeOf a' = case a' of
    UA _ _ c     -> typeOf $ head c
    UAExtern _ t -> t

instance TypeOf (A a) where
  typeOf (A ua) = typeOf ua

instance TypeOf UE where
  typeOf t' = case t' of
    UVRef     uvar  -> typeOf uvar
    UCast     t _   -> t
    UConst    c     -> typeOf c
    UAdd      a _   -> typeOf a
    USub      a _   -> typeOf a
    UMul      a _   -> typeOf a
    UDiv      a _   -> typeOf a
    UMod      a _   -> typeOf a
    UNot      _     -> Bool
    UAnd      _     -> Bool
    UBWNot    a     -> typeOf a
    UBWAnd    a _   -> typeOf a
    UBWOr     a _   -> typeOf a
    UBWXor    a _   -> typeOf a
    UBWShiftL a _   -> typeOf a
    UBWShiftR a _   -> typeOf a
    UEq       _ _   -> Bool
    ULt       _ _   -> Bool
    UMux      _ a _ -> typeOf a
    UF2B      _     -> Word32
    UD2B      _     -> Word64
    UB2F      _     -> Float
    UB2D      _     -> Double
-- math.h:
    UPi           -> Double
    UExp      a     -> typeOf a
    ULog      a     -> typeOf a
    USqrt     a     -> typeOf a
    UPow      a _   -> typeOf a
    USin      a     -> typeOf a
    UAsin     a     -> typeOf a
    UCos      a     -> typeOf a
    UAcos     a     -> typeOf a
    USinh     a     -> typeOf a
    UCosh     a     -> typeOf a
    UAsinh    a     -> typeOf a
    UAcosh    a     -> typeOf a
    UAtan     a     -> typeOf a
    UAtanh    a     -> typeOf a

instance Expr a => TypeOf (E a) where
  typeOf = eType

-- | Typed expression:
class Eq a => Expr a where
  eType      :: E a -> Type
  constant   :: a -> Const
  expression :: E a -> Expression
  variable   :: V a -> Variable
  rawBits    :: E a -> E Word64

instance Expr Bool   where
  eType _    = Bool
  constant   = CBool
  expression = EBool
  variable   = VBool
  rawBits a  = mux a 1 0

instance Expr Int8   where
  eType _    = Int8
  constant   = CInt8
  expression = EInt8
  variable   = VInt8
  rawBits    = Cast

instance Expr Int16  where
  eType _    = Int16
  constant   = CInt16
  expression = EInt16
  variable   = VInt16
  rawBits    = Cast

instance Expr Int32  where
  eType _    = Int32
  constant   = CInt32
  expression = EInt32
  variable   = VInt32
  rawBits     = Cast

instance Expr Int64  where
  eType _    = Int64
  constant   = CInt64
  expression = EInt64
  variable   = VInt64
  rawBits    = Cast

instance Expr Word8  where
  eType _    = Word8
  constant   = CWord8
  expression = EWord8
  variable   = VWord8
  rawBits    = Cast

instance Expr Word16 where
  eType _    = Word16
  constant   = CWord16
  expression = EWord16
  variable   = VWord16
  rawBits    = Cast

instance Expr Word32 where
  eType _    = Word32
  constant   = CWord32
  expression = EWord32
  variable   = VWord32
  rawBits    = Cast

instance Expr Word64 where
  eType _    = Word64
  constant   = CWord64
  expression = EWord64
  variable   = VWord64
  rawBits    = id

instance Expr Float  where
  eType _    = Float
  constant   = CFloat
  expression = EFloat
  variable   = VFloat
  rawBits    = Cast . F2B

instance Expr Double where
  eType _    = Double
  constant   = CDouble
  expression = EDouble
  variable   = VDouble
  rawBits    = D2B

-- | Expression of numerical type
class (Num a, Expr a, EqE a, OrdE a) => NumE a
instance NumE Int8
instance NumE Int16
instance NumE Int32
instance NumE Int64
instance NumE Word8
instance NumE Word16
instance NumE Word32
instance NumE Word64
instance NumE Float
instance NumE Double

-- | Expression of integral type
class (NumE a, Integral a) => IntegralE a where
  -- | Returns True if expression is signed, and False if unsigned.
  signed :: E a -> Bool

instance IntegralE Int8   where signed _ = True
instance IntegralE Int16  where signed _ = True
instance IntegralE Int32  where signed _ = True
instance IntegralE Int64  where signed _ = True
instance IntegralE Word8  where signed _ = False
instance IntegralE Word16 where signed _ = False
instance IntegralE Word32 where signed _ = False
instance IntegralE Word64 where signed _ = False

-- | Expressions which can be compared for equality
class (Eq a, Expr a) => EqE a
instance EqE Bool
instance EqE Int8
instance EqE Int16
instance EqE Int32
instance EqE Int64
instance EqE Word8
instance EqE Word16
instance EqE Word32
instance EqE Word64
instance EqE Float
instance EqE Double

-- | Expressions which can be ordered
class (Eq a, Ord a, EqE a) => OrdE a
instance OrdE Int8
instance OrdE Int16
instance OrdE Int32
instance OrdE Int64
instance OrdE Word8
instance OrdE Word16
instance OrdE Word32
instance OrdE Word64
instance OrdE Float
instance OrdE Double

-- | Floating-point typed expression
class (RealFloat a, NumE a, OrdE a) => FloatingE a
instance FloatingE Float
instance FloatingE Double

instance (Num a, NumE a, OrdE a) => Num (E a) where
  (Const a) + (Const b) = Const $ a + b
  a + b = Add a b
  (Const a) - (Const b) = Const $ a - b
  a - b = Sub a b
  (Const a) * (Const b) = Const $ a * b
  a * b = Mul a b
  negate a = 0 - a
  abs a = mux (a <. 0) (negate a) a
  signum a = mux (a ==. 0) 0 $ mux (a <. 0) (-1) 1
  fromInteger = Const . fromInteger

instance (OrdE a, NumE a, Num a, Fractional a) => Fractional (E a) where
  (Const a) / (Const b) = Const $ a / b
  a / b = Div a b
  recip a = 1 / a
  fromRational r = Const $ fromInteger (numerator r) / fromInteger (denominator r)

-- make typed Atom expressions an instance of Floating
-- to generate calls to functions in math.h
instance (Num a, Fractional a, Floating a, FloatingE a) => Floating (E a) where
  pi       = Const pi
  exp      (Const a) = Const $ exp a
  exp      a         = Exp a
  log      (Const a) = Const $ log a
  log      a         = Log a
  sqrt     (Const a) = Const $ sqrt a
  sqrt     a         = Sqrt a
  (**)     (Const a) (Const b) = Const $ a ** b
  (**)     a b       = Pow a b
  sin      (Const a) = Const $ sin a
  sin      a         = Sin a
  cos      a         = sqrt (1 - sin a ** 2)
  sinh     a         = (exp a - exp (-a)) / 2
  cosh     a         = (exp a + exp (-a)) / 2
  asin     (Const a) = Const $ asin a
  asin     a         = Asin a
  acos     a         = pi / 2 - asin a
  atan     a         = asin (a / (sqrt (a ** 2 + 1)))
  asinh    a         = log (a + sqrt (a ** 2 + 1))
  acosh    a         = log (a + sqrt (a ** 2 - 1))
  atanh    a         = 0.5 * log ((1 + a) / (1 - a))

instance (Expr a, OrdE a, EqE a, IntegralE a, Bits a) => Bits (E a) where
  (Const a) .&. (Const b) = Const $ a .&. b
  a .&. b                 = BWAnd a b
  complement (Const a)    = Const $ complement a
  complement a            = BWNot a
  (Const a) .|. (Const b) = Const $ a .|. b
  a .|. b                 = BWOr  a b
  xor                     = BWXor

  shiftL _ _              = error "shiftL undefined, for left-shifting use .<<."
  shiftR _ _              = error "shiftR undefined, for right-shifting use .>>."
  bitSizeMaybe            = error "bitSizeMaybe undefined"
  testBit                 = error "testBit undefinied"
  bit                     = error "bit undefinied"
  popCount                = error "popCount undefined"

  rotateL _ _             = error "rotateL undefined, for left-rotation use rol"
  rotateR _ _             = error "rotateR undefined, for right-rotation use ror"
  bitSize = width
  isSigned = signed

-- | Bitwise left-shifting.
(.<<.) :: ( Bits a, IntegralE a, IntegralE n ) => E a -> E n -> E a
( Const a ) .<<. ( Const n ) = Const $ shiftL a $ fromIntegral n
a .<<. n                     = BWShiftL a n

-- | Bitwise right-shifting.
(.>>.) :: ( Bits a, IntegralE a, IntegralE n ) => E a -> E n -> E a
( Const a ) .>>. ( Const n ) =  Const $ shiftR a $ fromIntegral n
a .>>. n = BWShiftR a n

-- | Bitwise left-rotation.
rol :: (IntegralE a, IntegralE n, Bits a) => E a -> E n -> E a
rol (Const a) (Const n) = Const $ rotateL a $ fromIntegral n
rol a n = a .<<. n .|. a .>>. ( ( Const . fromIntegral . width ) a - n )

-- | Bitwise right-rotation.
ror :: (IntegralE a, IntegralE n, Bits a) => E a -> E n -> E a
ror (Const a) (Const n) = Const $ rotateR a $ fromIntegral n
ror a n = a .>>. n .|. a .<<. ( ( Const . fromIntegral . width ) a - n )

-- | True term.
true :: E Bool
true = Const True

-- | False term.
false :: E Bool
false = Const False

-- | Logical negation.
not_ :: E Bool -> E Bool
not_ = Not

-- | Logical AND.
(&&.) :: E Bool -> E Bool -> E Bool
(&&.) = And

-- | Logical OR.
(||.) :: E Bool -> E Bool -> E Bool
(||.) a b = not_ $ not_ a &&. not_ b

-- | The conjunction of a E Bool list.
and_ :: [E Bool] -> E Bool
and_ = foldl (&&.) true

-- | The disjunction of a E Bool list.
or_ :: [E Bool] -> E Bool
or_ = foldl (||.) false

-- | True iff the predicate is true for all elements.
all_ :: (a -> E Bool) -> [a] -> E Bool
all_ f a = and_ $ map f a

-- | True iff the predicate is true for any element.
any_ :: (a -> E Bool) -> [a] -> E Bool
any_ f a = or_ $ map f a

-- | Logical implication (if a then b).
imply :: E Bool -- ^ a
         -> E Bool -- ^ b
         -> E Bool
imply a b = not_ a ||. b

-- | Equal.
(==.) :: EqE a => E a -> E a -> E Bool
(==.) = Eq

-- | Not equal.
(/=.) :: EqE a => E a -> E a -> E Bool
a /=. b = not_ (a ==. b)

-- | Less than.
(<.) :: OrdE a => E a -> E a -> E Bool
(<.) = Lt

-- | Greater than.
(>.) :: OrdE a => E a -> E a -> E Bool
a >. b = b <. a

-- | Less than or equal.
(<=.) :: OrdE a => E a -> E a -> E Bool
a <=. b =  not_ (a >. b)

-- | Greater than or equal.
(>=.) :: OrdE a => E a -> E a -> E Bool
a >=. b = not_ (a <. b)

-- | Returns the minimum of two numbers.
min_ :: OrdE a => E a -> E a -> E a
min_ a b = mux (a <=. b) a b

-- | Returns the minimum of a list of numbers.
minimum_ :: OrdE a => [E a] -> E a
minimum_ = foldl1 min_

-- | Returns the maximum of two numbers.
max_ :: OrdE a => E a -> E a -> E a
max_ a b = mux (a >=. b) a b

-- | Returns the maximum of a list of numbers.
maximum_ :: OrdE a => [E a] -> E a
maximum_ = foldl1 max_

-- | Limits between min and max.
limit :: OrdE a => E a -> E a -> E a -> E a
limit a b i = max_ min' $ min_ max' i
  where
  min' = min_ a b
  max' = max_ a b

-- | Division.  If both the dividend and divisor are constants, a compile-time
-- check is made for divide-by-zero.  Otherwise, if the divisor ever evaluates
-- to @0@, a runtime exception will occur, even if the division occurs within
-- the scope of a 'cond' or 'mux' that tests for @0@ (because Atom generates
-- deterministic-time code, every branch of a 'cond' or 'mux' is executed).
div_ :: IntegralE a => E a -> E a -> E a
div_ (Const a) (Const b) = Const $ a `div` b
div_ a b = Div a b

-- | Division, where the C code is instrumented with a runtime check to ensure
-- the divisor does not equal @0@.  If it is equal to @0@, the 3rd argument is a
-- user-supplied non-zero divsor.
div0_ :: IntegralE a => E a -> E a -> a -> E a
div0_ _ _ 0 = error "The third argument to div0_ must be non-zero."
div0_ a b c = div_ a $ mux (b ==. 0) (Const c) b

-- | Modulo.  If both the dividend and modulus are constants, a compile-time
-- check is made for divide-by-zero.  Otherwise, if the modulus ever evaluates
-- to @0@, a runtime exception will occur, even if the division occurs within
-- the scope of a 'cond' or 'mux' that tests for @0@ (because Atom generates
-- deterministic-time code, every branch of a 'cond' or 'mux' is executed).
mod_ :: IntegralE a => E a -> E a -> E a
mod_ (Const a) (Const b) = Const $ a `mod` b
mod_ a b = Mod a b

-- | Modulus, where the C code is instrumented with a runtime check to ensure
-- the modulus does not equal @0@.  If it is equal to @0@, the 3rd argument is
-- a user-supplied non-zero divsor.
mod0_ :: IntegralE a => E a -> E a -> a -> E a
mod0_ _ _ 0 = error "The third argument to mod0_ must be non-zero."
mod0_ a b c = mod_ a $ mux (b ==. 0) (Const c) b

-- | Returns the value of a 'V'.
value :: V a -> E a
value = VRef

-- | Conditional expression.  Note, both branches are evaluated!
--
-- > mux test onTrue onFalse
mux :: Expr a => E Bool -> E a -> E a -> E a
mux = Mux

-- | Array index to variable.
(!) :: (Expr a, IntegralE b) => A a -> E b -> V a
(!) (A ua) = V . UVArray ua . ue

-- | Array index to expression.
(!.) :: (Expr a, IntegralE b) => A a -> E b -> E a
a !. i = value $ a ! i


-- | Converts an typed expression (E a) to an untyped expression (UE).
ue :: Expr a => E a -> UE
ue t = case t of
  VRef     (V v)   -> UVRef v
  Const    a     -> UConst  $ constant a
  Cast     a     -> UCast     tt (ue a)
  Add      a b   -> UAdd      (ue a) (ue b)
  Sub      a b   -> USub      (ue a) (ue b)
  Mul      a b   -> UMul      (ue a) (ue b)
  Div      a b   -> UDiv      (ue a) (ue b)
  Mod      a b   -> UMod      (ue a) (ue b)
  Not      a     -> unot      (ue a)
  And      a b   -> uand      (ue a) (ue b)
  BWNot    a     -> UBWNot    (ue a)
  BWAnd    a b   -> UBWAnd    (ue a) (ue b)
  BWOr     a b   -> UBWOr     (ue a) (ue b)
  BWXor    a b   -> UBWXor    (ue a) (ue b)
  BWShiftL a b   -> UBWShiftL (ue a) (ue b)
  BWShiftR a b   -> UBWShiftR (ue a) (ue b)
  Eq       a b   -> ueq       (ue a) (ue b)
  Lt       a b   -> ult       (ue a) (ue b)
  Mux      a b c -> umux      (ue a) (ue b) (ue c)
  F2B      a     -> UF2B      (ue a)
  D2B      a     -> UD2B      (ue a)
  B2F      a     -> UB2F      (ue a)
  B2D      a     -> UB2D      (ue a)
  Retype   a     -> a
-- math.h:
  Pi           -> UPi
  Exp      a     -> UExp      (ue a)
  Log      a     -> ULog      (ue a)
  Sqrt     a     -> USqrt     (ue a)
  Pow      a b   -> UPow      (ue a) (ue b)
  Sin      a     -> USin      (ue a)
  Asin     a     -> UAsin     (ue a)
  Cos      a     -> UCos      (ue a)
  Acos     a     -> UAcos     (ue a)
  Sinh     a     -> USinh     (ue a)
  Cosh     a     -> UCosh     (ue a)
  Asinh    a     -> UAsinh    (ue a)
  Acosh    a     -> UAcosh    (ue a)
  Atan     a     -> UAtan     (ue a)
  Atanh    a     -> UAtanh    (ue a)

  where
  tt = eType t

-- | Convert a typed variable to an untyped one
uv :: V a -> UV
uv (V v) = v

-- XXX A future smart constructor for numeric type casting.
-- ucast :: Type -> UE -> UE

-- | Produced an untyped expression from a constant 'Bool'
ubool :: Bool -> UE
ubool = UConst . CBool

-- | Logical NOT of an untyped expression
unot :: UE -> UE
unot (UConst (CBool a)) = ubool $ not a
unot (UNot a) = a
unot a = UNot a

-- | Logical AND of two untyped expressions
uand :: UE -> UE -> UE
uand a b | a == b                   = a
uand a@(UConst (CBool False)) _     = a
uand _ a@(UConst (CBool False))     = a
uand (UConst (CBool True)) a        = a
uand a (UConst (CBool True))        = a
uand (UAnd a) (UAnd b)              = reduceAnd $ a ++ b
uand (UAnd a) b                     = reduceAnd $ b : a
uand a (UAnd b)                     = reduceAnd $ a : b
uand a b                            = reduceAnd [a, b]

reduceAnd :: [UE] -> UE

-- a && not a
reduceAnd terms | not $ null [ e | e <- terms, e' <- map unot terms, e == e' ] = ubool False

-- a == x && a == y && x /= y
reduceAnd terms | or [ f a b | a <- terms, b <- terms ]                        = ubool False
  where
  f :: UE -> UE -> Bool
  f (UEq a b) (UEq x y) | a == x = yep $ ueq b y
                        | a == y = yep $ ueq b x
                        | b == x = yep $ ueq a y
                        | b == y = yep $ ueq a x
  f _ _ = False
  yep :: UE -> Bool
  yep (UConst (CBool False)) = True
  yep _ = False

-- a && b && not (a && b)
reduceAnd terms | not $ null [ e | e <- terms, not $ null $ f e, all (flip elem terms) $ f e ] = ubool False
  where
  f :: UE -> [UE]
  f (UNot (UAnd a)) = a
  f _               = []

-- collect, sort, and return
reduceAnd terms = UAnd $ sort $ nub terms

-- | Logical OR of two untyped expressions
uor :: UE -> UE -> UE
uor a b = unot (uand (unot a) (unot b))

-- | Check equality on two untyped expressions
ueq :: UE -> UE -> UE
ueq a b | a == b = ubool True
ueq (UConst (CBool   a)) (UConst (CBool   b)) = ubool $ a == b
ueq (UConst (CInt8   a)) (UConst (CInt8   b)) = ubool $ a == b
ueq (UConst (CInt16  a)) (UConst (CInt16  b)) = ubool $ a == b
ueq (UConst (CInt32  a)) (UConst (CInt32  b)) = ubool $ a == b
ueq (UConst (CInt64  a)) (UConst (CInt64  b)) = ubool $ a == b
ueq (UConst (CWord8  a)) (UConst (CWord8  b)) = ubool $ a == b
ueq (UConst (CWord16 a)) (UConst (CWord16 b)) = ubool $ a == b
ueq (UConst (CWord32 a)) (UConst (CWord32 b)) = ubool $ a == b
ueq (UConst (CWord64 a)) (UConst (CWord64 b)) = ubool $ a == b
ueq (UConst (CFloat  a)) (UConst (CFloat  b)) = ubool $ a == b
ueq (UConst (CDouble a)) (UConst (CDouble b)) = ubool $ a == b
ueq a b = UEq a b

-- | Less-than inequality on two untyped expressions
ult :: UE -- ^ a
       -> UE -- ^ b
       -> UE -- ^ a < b
ult a b | a == b = ubool False
ult (UConst (CBool   a)) (UConst (CBool   b)) = ubool $ a < b
ult (UConst (CInt8   a)) (UConst (CInt8   b)) = ubool $ a < b
ult (UConst (CInt16  a)) (UConst (CInt16  b)) = ubool $ a < b
ult (UConst (CInt32  a)) (UConst (CInt32  b)) = ubool $ a < b
ult (UConst (CInt64  a)) (UConst (CInt64  b)) = ubool $ a < b
ult (UConst (CWord8  a)) (UConst (CWord8  b)) = ubool $ a < b
ult (UConst (CWord16 a)) (UConst (CWord16 b)) = ubool $ a < b
ult (UConst (CWord32 a)) (UConst (CWord32 b)) = ubool $ a < b
ult (UConst (CWord64 a)) (UConst (CWord64 b)) = ubool $ a < b
ult (UConst (CFloat  a)) (UConst (CFloat  b)) = ubool $ a < b
ult (UConst (CDouble a)) (UConst (CDouble b)) = ubool $ a < b
ult a b = ULt a b

-- | 2-to-1 multiplexer. If selector is true, this returns input 1; if
-- selector is false, this returns input 2.
umux :: UE -- ^ Selector
        -> UE -- ^ Input 1
        -> UE -- ^ Input 2
        -> UE
umux _ t f | t == f = f
umux b t f | typeOf t == Bool = uor (uand b t) (uand (unot b) f)
umux (UConst (CBool b)) t f = if b then t else f
umux (UNot b) t f = umux b f t
umux b1 (UMux b2 t _) f | b1 == b2 = umux b1 t f
umux b1 t (UMux b2 _ f) | b1 == b2 = umux b1 t f
umux b t f = UMux b t f

{-
-- | Balances mux trees in expression.  Reduces critical path at cost of additional logic.
balance :: UE -> UE
balance ue = case ue of
  UVRef _      -> ue
  UCast t a    -> UCast t (balance a)
  UConst _     -> ue
  UAdd a b     -> UAdd   (balance a) (balance b)
  USub a b     -> USub   (balance a) (balance b)
  UMul a b     -> UMul   (balance a) (balance b)
  UDiv a b     -> UDiv   (balance a) (balance b)
  UMod a b     -> UMod   (balance a) (balance b)
  UNot a       -> UNot   (balance a)
  UAnd a       -> UAnd   (map balance a)
  UBWNot a     -> UBWNot (balance a)
  UBWAnd a b   -> UBWAnd (balance a) (balance b)
  UBWOr  a b   -> UBWOr  (balance a) (balance b)
  UShift a b   -> UShift (balance a) (balance b)
  UEq  a b     -> UEq    (balance a) (balance b)
  ULt  a b     -> ULt    (balance a) (balance b)
  UMux a t f   -> rotate $ umux a t' f'
    where
    t' = balance t
    f' = balance f
    depth :: UE -> Int
    depth (UMux _ t f) = 1 + max (depth t) (depth f)
    depth _            = 0
    rotate :: UE -> UE
    rotate ue = case ue of
      UMux a1 t1@(UMux a2 t2 f2) f1 | depth t1 >= depth f1 + 2 -> umux (uand a1 a2) t2 (umux a1 f2 f1)
      UMux a1 t1 f1@(UMux a2 t2 f2) | depth f1 >= depth t1 + 2 -> umux (uor  a1 a2) (umux a1 t1 t2) f2
      _ -> ue
-}

-- Idea analyzing a pair of comparisons with one common operand: take to other two operands and construct the appropriate
-- expression and check never.
-- never (a == x) && (a == y)  =>  never (x == y)
-- never (a == x) && (a <  y)  =>  never (x >= y)
-- never (a == x) && (a /= y)  =>  never (x == y)
-- never (a == x) || (a == y)  =>  never (x == y)
{-
isExclusiveCompare :: (ConstantCompare, ConstantCompare) -> Bool
isExclusiveCompare a = case a of
  (Equal a, Equal     b) -> a /= b
  (Equal a, NotEqual  b) -> a == b
  (Equal a, Less      b) -> a >= b
  (Equal a, LessEqual b) -> a >  b
  (Equal a, More      b) -> a <= b
  (Equal a, MoreEqual b) -> a <  b

  (NotEqual a, Equal     b) -> a == b
  (NotEqual _, _)           -> False

  (Less  a, Equal     b) -> a <= b
  (Less  a, More      b) -> a <= b
  (Less  a, MoreEqual b) -> a <= b
  (Less  _, _)           -> False

  (LessEqual  a, Equal     b) -> a <  b
  (LessEqual  a, More      b) -> a <= b
  (LessEqual  a, MoreEqual b) -> a <  b
  (LessEqual  _, _)           -> False

  (More  a, Equal     b) -> a >= b
  (More  a, Less      b) -> a >= b
  (More  a, LessEqual b) -> a >= b
  (More  _, _)           -> False

  (MoreEqual  a, Equal     b) -> a >  b
  (MoreEqual  a, Less      b) -> a >= b
  (MoreEqual  a, LessEqual b) -> a >  b
  (MoreEqual  _, _)           -> False

data ConstantCompare
  = Equal     TermConst
  | NotEqual  TermConst
  | Less      TermConst
  | LessEqual TermConst
  | More      TermConst
  | MoreEqual TermConst
  deriving (Eq, Ord)
-}

--data NetList = NetList Int (IntMap UE)
