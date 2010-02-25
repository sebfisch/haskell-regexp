{-# LANGUAGE RankNTypes, BangPatterns, GeneralizedNewtypeDeriving #-}

module Text.RegExp.Matcher where

import Data.Monoid

import Data.Set ( Set )
import qualified Data.Set as Set

import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq

import qualified Data.Foldable as Fold

import Data.Semiring
import Text.RegExp.Data

-- | Subwords of words that match a regular expression are represented
--   as values of type 'Matching'.
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

-- | Checks whether a regular expression matches the given word. For
--   example, @accept (fromString \"b|abc\") \"b\"@ yields @True@
--   because the first alternative of @b|abc@ matches the string
--   @\"b\"@.
-- 
accept :: (forall w. Semiring w => RegExp w a) -> [a] -> Bool
accept r = wholeWord r

-- | Checks whether a regular expression matches a subword of the
--   given word. For example, @accept (fromString \"b|abc\") \"ab\"@
--   yields @True@ because the first alternative of @b|abc@ matches a
--   subword of @\"ab\"@. Note that regular expressions that accept
--   the empty word accept a subword of every word.
-- 
acceptSubword :: (forall w. Semiring w => RegExp w a) -> [a] -> Bool
acceptSubword r xs = empty r || subWords r xs

-- | Computes in how many ways a word can be matched against a regular
--   expression.
-- 
matchingCount :: (forall w. Semiring w => RegExp w a) -> [a] -> Int
matchingCount r = wholeWord r

newtype Match = Match { getMatch :: (First Int, Last Int) }
 deriving (Eq,Monoid)

-- leftmost longest match is the smallest
instance Ord Match
 where compare (Match a) (Match b) =
         case (getFirst (fst a), getFirst (fst b)) of
           (Nothing,Nothing) -> compare (snd b) (snd a)
           (Nothing,_      ) -> GT
           (_      ,Nothing) -> LT
           (Just i ,Just j ) -> compare (i,snd b) (j,snd a)

match :: Int -> Int -> Match
match i j = Match (First (Just i), Last (Just j))

fromMatch :: Match -> Matching
fromMatch (Match (First (Just i), Last (Just j))) = Matching i j

-- | Returns a list of all non-empty matchings for a regular
--   expression in a given word. A matching is a pair of two numbers,
--   where the first is the index (>= 0) where the matched subword
--   starts and the second is the length (>= 1) of the matched
--   subword.
-- 
allMatchings :: (forall w. Semiring w => RegExp w a) -> [a] -> [Matching]
allMatchings r = map fromMatch . Set.toList
               . subWords (weightSymbols (\i _ -> Set.singleton (match i i)) r)

-- | Returns the leftmost longest of all non-empty matchings for a
--   regular expression in a given word. @firstMatching r@ computes
--   the same result as @head . allMatchings r@ but more efficiently.
-- 
firstMatching :: (forall w. Semiring w => RegExp w a) -> [a] -> Maybe Matching
firstMatching r = fmap fromMatch . getMin
                . subWords (weightSymbols (\i _ -> Min (Just (match i i))) r)

-- | Returns the list of all subwords of a word that match the given
--   regular expression in lexicographical (alphabetical) order.
-- 
allMatchingWords :: Ord a => (forall w. Semiring w => RegExp w a)
                          -> [a] -> [[a]]
allMatchingWords r =
  map Fold.toList . Set.toList
  . subWords (weightSymbols (\_ a -> Set.singleton (Seq.singleton a)) r)

-- | Returns the leftmost longest subword of a word that matches the
--   given regular expression.
-- 
firstMatchingWord :: Ord a => (forall w. Semiring w => RegExp w a)
                           -> [a] -> Maybe [a]
firstMatchingWord r =
  fmap (Fold.toList . snd) . getMin
  . subWords (weightSymbols (\i a -> Min (Just (match i i, Seq.singleton a))) r)

-- matching algorithm

wholeWord :: Semiring w => RegExp w a -> [a] -> w
wholeWord r []     = empty r
wholeWord r (x:xs) = final . first
                   . foldl (next zero) (next one (Pair r 0) x) $ xs

subWords :: Semiring w => RegExp w a -> [a] -> w
subWords r = foldr (.+.) zero . map (final . first)
           . scanl (next one) (Pair r 0)

next :: Semiring w => w -> Pair (RegExp w a) Int -> a -> Pair (RegExp w a) Int
next w (Pair x n) a = Pair (pass w x) (n+1)
 where
  pass t = shift t . regExp

  shift _ (Weight w)   = weight w
  shift t (Symbol s p) = RegExp zero (t .*. p n a) (Symbol s p)
  shift t (Star r)     = star (pass (t .+. final r) r)
  shift t (r :*: s)    = pass t r .*. pass (t .*. empty r .+. final r) s
  shift t (r :+: s)    = pass t r .+. pass t s

-- strict pairs for speed

data Pair a b = Pair !a !b

first (Pair x _) = x
