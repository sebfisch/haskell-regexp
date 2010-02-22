{-# LANGUAGE RankNTypes #-}

module Text.RegExp.Matcher where

import Data.Monoid

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Semiring
import Text.RegExp.Data

-- | Subwords of words that match a regular expression are represented
--   as values of type 'Matching'.
-- 
data Matching = Matching {
 
  -- | Index of the matching subword in the queried word.
  matchingIndex :: Int,
 
  -- | Length of the matching subword.
  matchingLength :: Int
 
  }
 
instance Show Matching
 where
  showsPrec _ m = showString "<at:" . shows (matchingIndex m)
                . showString " len:" . shows (matchingLength m)
                . showString ">"
 
  showList = showString . unlines . map show

-- | Checks whether a regular expression matches the given word. For
--   example, @accept (fromString \"b|abc\") \"b\"@ yields @True@
--   because the first alternative of @b|abc@ matches the string
--   @\"b\"@.
-- 
accept :: (forall s. Semiring s => RegExp s a) -> [a] -> Bool
accept r = process r

-- | Computes in how many ways a word can be matched against a regular
--   expression.
-- 
matchingCount :: (forall s. Semiring s => RegExp s a) -> [a] -> Int
matchingCount r = process r

-- | Returns a list of all non-empty matchings for a regular
--   expression in a given word. A matching is a pair of two numbers,
--   where the first is the index (>= 0) where the matched subword
--   starts and the second is the length (>= 1) of the matched
--   subword.
-- 
allMatchings :: (forall s. Semiring s => RegExp s a) -> [a] -> [Matching]
allMatchings r = map (matching . map getSum . fromTuple) . Set.toList
               . process (weightSymbols (subLengths [1])
                          (symbols .*. delim .*. r .*. delim .*. symbols))
 where
  symbols          = star anySymbol
  delim            = weight (subLengths [0,0])
  matching [i,l,_] = Matching i l

subLengths :: [a] -> Set (Tuple (Sum a))
subLengths = Set.singleton . tuple . map Sum

-- matching algorithm

process :: Semiring s => RegExp s a -> [a] -> s
process r []     = empty r
process r (x:xs) = final $ foldl (next zero) (next one r x) xs

next :: Semiring s => s -> RegExp s a -> a -> RegExp s a
next t x a = pass $ regExp x
 where
  pass (Weight w)   = weight w
  pass (Symbol s p) = RegExp zero (if p a then t else zero) (Symbol s p)
  pass (Star r)     = star (next (t .+. final r) r a)
  pass (r :*: s)    = next t r a .*. next (t .*. empty r .+. final r) s a
  pass (r :+: s)    = next t r a .+. next t s a
