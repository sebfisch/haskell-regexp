-- | 
-- Module      : Data.Semiring.Properties
-- Copyright   : Sebastian Fischer <mailto:mail@sebfisch.de>
-- License     : BSD3
-- 
-- This library provides properties for the 'Semiring' type class that
-- can be checked using libraries like QuickCheck or SmallCheck.
-- 
module Data.Semiring.Properties (

  module Data.Semiring, module Data.Semiring.Properties

  ) where

import Data.Semiring

-- | > a .+. b  ==  b .+. a
plus'comm :: Semiring s => s -> s -> Bool
plus'comm a b  =  a .+. b  ==  b .+. a

-- | > zero .+. a  ==  a
left'zero :: Semiring s => s -> Bool
left'zero a  =  zero .+. a  ==  a

-- | > (a .+. b) .+. c  ==  a .+. (b .+. c)
add'assoc :: Semiring s => s -> s -> s -> Bool
add'assoc a b c  =  (a .+. b) .+. c  ==  a .+. (b .+. c)

-- | > one .*. a  ==  a
left'one :: Semiring s => s -> Bool
left'one a  =  one .*. a  ==  a

-- | > a .*. one  ==  a
right'one :: Semiring s => s -> Bool
right'one a  =  a .*. one  ==  a

-- | > (a .*. b) .*. c  ==  a .*. (b .*. c)
mul'assoc :: Semiring s => s -> s -> s -> Bool
mul'assoc a b c  =  (a .*. b) .*. c  ==  a .*. (b .*. c)

-- | > a .*. (b .+. c)  ==  (a .*. b) .+. (a .*. c)
left'distr :: Semiring s => s -> s -> s -> Bool
left'distr a b c  =  a .*. (b .+. c)  ==  (a .*. b) .+. (a .*. c)

-- | > (a .+. b) .*. c  ==  (a .*. c) .+. (b .*. c)
right'distr :: Semiring s => s -> s -> s -> Bool
right'distr a b c  =  (a .+. b) .*. c  ==  (a .*. c) .+. (b .*. c)

-- | > zero .*. a  ==  zero
left'ann :: Semiring s => s -> Bool
left'ann a  =  zero .*. a  ==  zero

-- | > a .*. zero  ==  zero
right'ann :: Semiring s => s -> Bool
right'ann a  =  a .*. zero  ==  zero
