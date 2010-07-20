{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Text.RegExp.Matching where

import Data.Semiring
import Text.RegExp.Data

-- |
-- Subwords of words that match a regular expression are represented
-- as values of type 'Matching'.
-- 
data Matching = Matching {
 
  -- | Start index of the matching subword in the queried word.
  matchingIndex :: Int,
 
  -- | Length of the matching subword.
  matchingLength :: Int
 
  }
 
instance Show Matching
 where
  showsPrec _ m = showString "<index:" . shows (matchingIndex m)
                . showString " length:" . shows (matchingLength m)
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

-- |
-- Matches a regular expression against a word computing a weight in
-- an arbitrary semiring.
-- 
match :: Semiring w => RegExp c -> [c] -> w
match (RegExp r) = matchW r

-- |
-- Matches a regular expression against substrings of a word computing
-- a weight in an arbitrary semiring. The 'symWeight' function of
-- 'Weight's is used to report positional information about the
-- matching part of the word to the semiring.
-- 
submatch :: Weight c (Int,c) w => RegExp c -> [c] -> w
submatch (RegExp r) =
  matchW (arb `seqW` weighted r `seqW` arb) . zip [(0::Int)..]
 where arb = repW (symW (const one))

matchW :: Semiring w => RegW w c -> [c] -> w
matchW r []     = empty r
matchW r (c:cs) = final (foldl (shiftW zero) (shiftW one r c) cs)

shiftW :: Semiring w => w -> RegW w c -> c -> RegW w c
shiftW w r c | active r || w /= zero = shift w (reg r) c
             | otherwise             = r

shift :: Semiring w => w -> Reg w c -> c -> RegW w c
shift _ Eps       _ = epsW
shift w (Sym f)   c = let w' = w .*. f c
                       in (symW f) { active = w' /= zero, final_ = w' }
shift w (Alt p q) c = altW (shiftW w p c) (shiftW w q c)
shift w (Seq p q) c = seqW (shiftW w p c)
                           (shiftW (w .*. empty p .+. final p) q c)
shift w (Rep r)   c = repW (shiftW (w .+. final r) r c)
