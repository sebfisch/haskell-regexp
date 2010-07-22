{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | 
-- Module      : Data.Semiring
-- Copyright   : Thomas Wilke, Frank Huch, Sebastian Fischer
-- License     : BSD3
-- Maintainer  : Sebastian Fischer <mailto:mail@sebfisch.de>
-- Stability   : experimental
-- 
-- This library provides a type class for semirings and instances for
-- standard data types.
-- 
module Data.Semiring (

  Semiring(..), fromBool,

  Numeric, numeric, getNumeric

  ) where

infixr 6 .+.
infixr 7 .*.

-- |
-- A semiring is an additive commutative monoid with identity 'zero':
-- 
-- >         a .+. b  ==  b .+. a
-- >      zero .+. a  ==  a
-- > (a .+. b) .+. c  ==  a .+. (b .+. c)
-- 
-- A semiring is a multiplicative monoid with identity 'one':
-- 
-- >        one .*. a  ==  a
-- >        a .*. one  ==  a
-- >  (a .*. b) .*. c  ==  a .*. (b .*. c)
-- 
-- Multiplication distributes over addition:
-- 
-- > a .*. (b .+. c)  ==  (a .*. b) .+. (a .*. c)
-- > (a .+. b) .*. c  ==  (a .*. c) .+. (b .*. c)
-- 
-- 'zero' annihilates a semiring with respect to multiplication:
-- 
-- > zero .*. a  ==  zero
-- > a .*. zero  ==  zero
-- 
-- All laws should hold with respect to the required `Eq` instance.
-- 
-- For example, the Booleans form a semiring.
-- 
--  * @False@ is an identity of disjunction which is commutative and
--    associative,
-- 
--  * @True@ is an identity of conjunction which is associative,
-- 
--  * conjunction distributes over disjunction, and
-- 
--  * @False@ annihilates the Booleans with respect to conjunction.
-- 
class Eq s => Semiring s where
  zero, one    :: s
  (.+.), (.*.) :: s -> s -> s

-- | Auxiliary function to convert Booleans to an arbitrary semiring.
-- 
fromBool :: Semiring s => Bool -> s
fromBool False = zero
fromBool True  = one

instance Semiring Bool where
  zero = False; one = True; (.+.) = (||); (.*.) = (&&)

-- |
-- Wrapper for numeric types.
-- 
-- Every numeric type that satisfies the semiring laws (as all
-- predefined numeric types do) is a semiring.
-- 
-- We represent zero and one explicitly to be able to define less
-- strict arithmetic functions.
-- 
data Numeric a = Zero | One | Num a
 deriving Eq

numeric :: Num a => a -> Numeric a
numeric 0 = Zero
numeric 1 = One
numeric n = Num n

getNumeric :: Num a => Numeric a -> a
getNumeric Zero    = 0
getNumeric One     = 1
getNumeric (Num n) = n

instance (Num a, Show a) => Show (Numeric a) where
  show = show . getNumeric

instance Functor Numeric where
  fmap _ Zero    = Zero
  fmap _ One     = One
  fmap f (Num n) = Num (f n)

lift2 :: Num a => (a -> a -> a) -> Numeric a -> Numeric a -> Numeric a
lift2 f x y = numeric (f (getNumeric x) (getNumeric y))

instance Num a => Num (Numeric a) where
  fromInteger = numeric . fromInteger
  signum      = fmap signum
  abs         = fmap abs

  Zero + x    = x
  x    + Zero = x
  x    + y    = lift2 (+) x y

  Zero * _    = Zero
  _    * Zero = Zero
  One  * x    = x
  x    * One  = x
  x    * y    = lift2 (*) x y

instance Num a => Semiring (Numeric a) where
  zero = 0; one = 1; (.+.) = (+); (.*.) = (*)
