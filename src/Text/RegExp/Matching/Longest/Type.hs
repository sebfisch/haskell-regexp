module Text.RegExp.Matching.Longest.Type where

import Data.Semiring
import Text.RegExp.Data

-- | Semiring used for longest matching.
-- 
data Longest = Zero | One | Longest !Int
 deriving (Eq,Show)

instance Semiring Longest where
  zero = Zero; one = One

  Zero       .+.  y          =  y
  x          .+.  Zero       =  x
  One        .+.  y          =  y
  x          .+.  One        =  x
  Longest a  .+.  Longest b  =  Longest (max a b)

  Zero       .*.  _          =  Zero
  _          .*.  Zero       =  Zero
  One        .*.  y          =  y
  x          .*.  One        =  x
  Longest a  .*.  Longest b  =  Longest (a+b)

instance Weight c c Longest where
  symWeight p c = p c .*. Longest 1
