{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Text.RegExp.Matching.Leftmost.Type where

import Data.Semiring
import Text.RegExp.Data

-- | Semiring used for leftmost matching.
-- 
data Leftmost = Zero | One | Leftmost !Int
 deriving (Eq,Show)

instance Semiring Leftmost where
  zero = Zero; one = One

  Zero        .+.  y           =  y
  x           .+.  Zero        =  x
  One         .+.  y           =  y
  x           .+.  One         =  x
  Leftmost a  .+.  Leftmost b  =  Leftmost (min a b)

  Zero        .*.  _           =  Zero
  _           .*.  Zero        =  Zero
  One         .*.  y           =  y
  x           .*.  One         =  x
  Leftmost a  .*.  Leftmost b  =  Leftmost (min a b)

instance Weight c (Int,c) Leftmost where
  symWeight p (n,c) = p c .*. Leftmost n
