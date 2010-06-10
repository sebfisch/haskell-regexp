{-# LANGUAGE RankNTypes, TypeOperators, GeneralizedNewtypeDeriving #-}

module Text.RegExp.Matcher where

import Data.Monoid

import qualified Data.Sequence as Seq; import Data.Sequence ( Seq )
import qualified Data.Foldable as Fold

import Data.Semiring
import Text.RegExp.Data

import Prelude hiding ( seq )

-- |
-- Subwords of words that match a regular expression are represented
-- as values of type 'Matching'.
-- 
data Matching = Matching {
 
  -- | First index of the matching subword in the queried word.
  firstIndex :: Int,
 
  -- | Last index of the matching subword.
  lastIndex :: Int
 
  }
 
instance Show Matching
 where
  showsPrec _ m = showString "<from:" . shows (firstIndex m)
                . showString " to:" . shows (lastIndex m)
                . showString ">"
 
  showList = showString . unlines . map show

-- |
-- Checks whether a regular expression matches the given word. For
-- example, @accept (fromString \"b|abc\") \"b\"@ yields @True@
-- because the first alternative of @b|abc@ matches the string
-- @\"b\"@.
-- 
accept :: RegExp c -> [c] -> Bool
accept r = match r

-- |
-- Computes in how many ways a word can be matched against a regular
-- expression.
-- 
matchingCount :: RegExp c -> [c] -> Int
matchingCount r = getNumeric . match r

newtype LL = LL { getLL :: (First Int, Last Int) }
 deriving (Eq,Monoid)

leftlong :: Int -> Int -> LL
leftlong i j = LL (First (Just i), Last (Just j))

fromLL :: LL -> Matching
fromLL (LL (First (Just i), Last (Just j))) = Matching i j

-- leftmost longest match is the smallest
instance Ord LL
 where compare (LL a) (LL b) =
         case (getFirst (fst a), getFirst (fst b)) of
           (Nothing,Nothing) -> compare (snd b) (snd a)
           (Nothing,_      ) -> GT
           (_      ,Nothing) -> LT
           (Just i ,Just j ) -> compare (i,snd b) (j,snd a)

-- |
-- Returns the leftmost longest of all non-empty matchings for a
-- regular expression in a given word.
-- 
leftmostLongest :: RegExp c -> [c] -> Maybe Matching
leftmostLongest r = fmap fromLL . getMin . submatch r

-- |
-- Matches a regular expression against a word computing a weight in
-- an arbitrary semiring.
-- 
match :: Semiring w => RegExp c -> [c] -> w
match (RegExp r) = matchW r

-- |
-- Matches a regular expression against substrings of a word computing
-- a weight in an arbitrary semiring. The 'index' function of
-- semirings is used to report positional information about the
-- matching part of the word to the semiring.
-- 
submatch :: Semiring w => RegExp c -> [c] -> w
submatch r = match (arb `seq` indexed r `seq` arb) . zip [0..]
 where arb = rep anySym

matchW :: Semiring w => RegW w c -> [c] -> w
matchW r []     = empty r
matchW r (c:cs) = final (foldl (shiftW zero) (shiftW one r c) cs)

shiftW :: Semiring w => w -> RegW w c -> c -> RegW w c
shiftW w r c | active r || w /= zero = shift w (reg r) c
             | otherwise             = r

shift :: Semiring w => w -> Reg w c -> c -> RegW w c
shift _ Eps       _ = epsW
shift w (Sym f)   c = (symW f) { final_ = w .*. f c }
shift w (Alt p q) c = altW (shiftW w p c) (shiftW w q c)
shift w (Seq p q) c = seqW (shiftW w p c)
                           (shiftW (w .*. empty p .+. final p) q c)
shift w (Rep r)   c = repW (shiftW (w .+. final r) r c)
