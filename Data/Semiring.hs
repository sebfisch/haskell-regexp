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
module Data.Semiring ( Semiring(..), fromBool, Numeric(..) ) where

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
newtype Numeric a = Numeric { getNumeric :: a } deriving (Eq,Show,Num)

instance Num a => Semiring (Numeric a) where
  zero = 0; one = 1; (.+.) = (+); (.*.) = (*)
