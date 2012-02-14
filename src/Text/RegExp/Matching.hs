{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Text.RegExp.Matching where

import Data.Semiring
import Text.RegExp.Data

import Text.RegExp.Matching.Leftmost.Type
import Text.RegExp.Matching.Longest.Type
import Text.RegExp.Matching.LeftLong.Type

-- |
-- Checks whether a regular expression matches the given word. For
-- example, @acceptFull (fromString \"b|abc\") \"b\"@ yields @True@
-- because the first alternative of @b|abc@ matches the string
-- @\"b\"@.
-- 
acceptFull :: RegExp c -> [c] -> Bool
acceptFull r = fullMatch r

-- |
-- Checks whether a regular expression matches a subword of the given
-- word. For example, @acceptPartial (fromString \"b\") \"abc\"@
-- yields @True@ because @\"abc\"@ contains the substring @\"b\"@.
-- 
acceptPartial :: RegExp c -> [c] -> Bool
acceptPartial r = partialMatch r

-- |
-- Computes in how many ways a word can be matched against a regular
-- expression.
-- 
matchingCount :: (Eq a, Num a) => RegExp c -> [c] -> a
matchingCount r = getNumeric . fullMatch r

{-# SPECIALIZE matchingCount :: RegExp c -> [c] -> Int #-}

-- |
-- Matches a regular expression against a word computing a weight in
-- an arbitrary semiring.
-- 
-- The symbols can have associated weights associated by the
-- 'symWeight' function of the 'Weight' class. This function also
-- allows to adjust the type of the used alphabet such that, for
-- example, positional information can be taken into account by
-- 'zip'ping the word with positions.
-- 
fullMatch :: Weight a b w => RegExp a -> [b] -> w
fullMatch (RegExp r) = matchW (weighted r)

{-# SPECIALIZE fullMatch :: RegExp c -> [c] -> Bool #-}
{-# SPECIALIZE fullMatch :: RegExp c -> [c] -> Numeric Int #-}
{-# SPECIALIZE fullMatch :: (Eq a, Num a) => RegExp c -> [c] -> Numeric a #-}
{-# SPECIALIZE fullMatch :: RegExp c -> [(Int,c)] -> Leftmost #-}
{-# SPECIALIZE fullMatch :: RegExp c -> [c] -> Longest #-}
{-# SPECIALIZE fullMatch :: RegExp c -> [(Int,c)] -> LeftLong #-}

-- |
-- Matches a regular expression against substrings of a word computing
-- a weight in an arbitrary semiring. Similar to 'fullMatch' the
-- 'Weight' class is used to associate weights to the symbols of the
-- regular expression.
-- 
partialMatch :: Weight a b w => RegExp a -> [b] -> w
partialMatch (RegExp r) = matchW (arb `seqW` weighted r `seqW` arb)
 where RegExp arb = rep anySym

{-# SPECIALIZE partialMatch :: RegExp c -> [c] -> Bool #-}
{-# SPECIALIZE partialMatch :: RegExp c -> [c] -> Numeric Int #-}
{-# SPECIALIZE partialMatch :: (Eq a, Num a) => RegExp c -> [c] -> Numeric a #-}
{-# SPECIALIZE partialMatch :: RegExp c -> [(Int,c)] -> Leftmost #-}
{-# SPECIALIZE partialMatch :: RegExp c -> [c] -> Longest #-}
{-# SPECIALIZE partialMatch :: RegExp c -> [(Int,c)] -> LeftLong #-}

matchW :: Semiring w => RegW w c -> [c] -> w
matchW r []     = empty r
matchW r (c:cs) = final (foldl (shiftW zero) (shiftW one r c) cs)

{-# SPECIALIZE matchW :: RegW Bool c -> [c] -> Bool #-}
{-# SPECIALIZE matchW :: RegW (Numeric Int) c -> [c] -> Numeric Int #-}
{-# SPECIALIZE matchW :: (Eq a, Num a) => RegW (Numeric a) c -> [c] -> Numeric a #-}
{-# SPECIALIZE matchW :: RegW Leftmost (Int,c) -> [(Int,c)] -> Leftmost #-}
{-# SPECIALIZE matchW :: RegW Longest c -> [c] -> Longest #-}
{-# SPECIALIZE matchW :: RegW LeftLong (Int,c) -> [(Int,c)] -> LeftLong #-}

shiftW :: Semiring w => w -> RegW w c -> c -> RegW w c
shiftW w r c | active r || w /= zero = shift w (reg r) c
             | otherwise             = r

shift :: Semiring w => w -> Reg w c -> c -> RegW w c
shift _ Eps       _ = epsW
shift w (Sym s f) c = let w' = w .*. f c
                       in (symW s f) { active = w' /= zero, final_ = w' }
shift w (Alt p q) c = altW (shiftW w p c) (shiftW w q c)
shift w (Seq p q) c = seqW (shiftW w p c)
                           (shiftW (w .*. empty p .+. final p) q c)
shift w (Rep r)   c = repW (shiftW (w .+. final r) r c)
