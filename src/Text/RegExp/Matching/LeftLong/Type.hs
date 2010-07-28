{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Text.RegExp.Matching.LeftLong.Type where

import Data.Semiring
import Text.RegExp.Data

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

