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
module Data.Semiring ( Semiring(..), fromBool, Numeric(..), Min(..) ) where

import Data.Monoid
import Control.Applicative

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

  -- | 
  -- The 'index' function is used to incorporate positional
  -- information into regular expression matching. It has a sensible
  -- default implementation. Semirings that do not need positional
  -- information need not define the 'index' function.
  -- 
  index :: Int -> s
  index _ = one

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

-- |
-- Adds a maximum element (infinity) to a totally ordered type.
-- 
-- A monoid with a total order is a semiring after adding a maximum
-- element. The (lifted) minimum operation serves as addition and the
-- underlying monoid is lifted into the multiplicative stucture.
-- 
-- The minimum operation is associative and commutative if the
-- underlying ordering is total (and antisymmetric):
-- 
-- > a <= b || b <= a
-- > a <= b && b <= a  ==>  a == b
-- 
-- The laws of the underlying monoid are preserved when lifting it
-- into the multiplicative structure.
-- 
-- The distributive laws require that @mappend@ distributes over @min@
-- in the underlying type:
-- 
-- > a `mappend` (b `min` c)  ==  (a `mappend` b) `min` (a `mappend` c)
-- > (a `min` b) `mappend` c  ==  (a `mappend` c) `min` (b `mappend` c)
-- 
-- By definition, 'zero' annihilates the semiring with respect to
-- multiplication.
-- 
newtype Min a = Min { getMin :: Maybe a } deriving Eq

instance (Ord a, Monoid a) => Semiring (Min a)
 where
  zero = Min Nothing
  one  = Min (Just mempty)

  a .+. b = Min (getMin a `plus` getMin b)
   where
    Nothing `plus` Nothing = Nothing
    Nothing `plus` Just y  = Just y
    Just x  `plus` Nothing = Just x
    Just x  `plus` Just y  = Just (min x y)

  a .*. b = Min $ liftA2 mappend (getMin a) (getMin b)
