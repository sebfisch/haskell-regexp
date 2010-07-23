{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Text.RegExp.Matching.LeftLong
-- Copyright   : Thomas Wilke, Frank Huch, and Sebastian Fischer
-- License     : BSD3
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- This module implements leftmost longest matching based on weighted
-- regular expressions. It should be imported qualified as the
-- interface resembles that provided by other matching modules.
-- 
module Text.RegExp.Matching.LeftLong (

  LeftLong(..), Matching(..),

  matching, getLeftLong

  ) where

import Text.RegExp

-- |
-- Subwords of words that match a regular expression are represented
-- as values of type 'Matching'.
-- 
data Matching = Matching {
 
  -- | Start index of the matching subword in the queried word.
  matchingIndex :: !Int,
 
  -- | Length of the matching subword.
  matchingLength :: !Int
 
  }
 deriving Eq

instance Show Matching
 where
  showsPrec _ m = showString "<index:" . shows (matchingIndex m)
                . showString " length:" . shows (matchingLength m)
                . showString ">"

-- |
-- Returns the leftmost longest of all non-empty matchings for a
-- regular expression in a given word. If the empty word is the only
-- matching its position is zero.
-- 
matching :: RegExp c -> [c] -> Maybe Matching
matching r = getLeftLong . partialMatch r

-- | 
-- Semiring used for leftmost longest matching.
-- 
-- The `LeftLong` type satisfies the distributive laws only with a
-- precondition on all involved multiplications: multiplied matches
-- must be adjacent and the start position must be smaller than the
-- end position. This precondition is satisfied for all
-- multiplications during regular expression matching.
-- 
data LeftLong = Zero | One | LeftLong !Int !Int
 deriving (Eq,Show)

getLeftLong :: LeftLong -> Maybe Matching
getLeftLong Zero            =  Nothing
getLeftLong One             =  Just $ Matching 0 0
getLeftLong (LeftLong x y)  =  Just $ Matching x (y-x+1)

instance Semiring LeftLong where
  zero = Zero; one = One

  Zero          .+.  y             =  y
  x             .+.  Zero          =  x
  One           .+.  y             =  y
  x             .+.  One           =  x
  LeftLong a b  .+.  LeftLong c d
    | a<c || a==c && b>=d          =  LeftLong a b
    | otherwise                    =  LeftLong c d

  Zero          .*.  _             =  Zero
  _             .*.  Zero          =  Zero
  One           .*.  y             =  y
  x             .*.  One           =  x
  LeftLong a _  .*.  LeftLong _ b  =  LeftLong a b

instance Weight c (Int,c) LeftLong where
  symWeight p (n,c) = p c .*. LeftLong n n

