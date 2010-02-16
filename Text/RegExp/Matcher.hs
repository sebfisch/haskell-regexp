module Text.RegExp.Matcher where

import Text.RegExp.Data

import Data.Monoid
import Data.Set ( Set, singleton, toList )

import Control.Monad

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

-- | Checks whether a regular expression matches (a subword of) the
--   given word. For example, @accept (fromString \"b|abc\") \"ab\"@
--   yields @True@ because the second character in the given string
--   can be matched against @b@.
-- 
accept :: RegExp Any a -> [a] -> Bool
accept r xs = isEmpty r || anyFinal (process r (zip (repeat (Any True)) xs))
 where anyFinal = getAny . mconcat . map activeLabel

-- | Returns a list of all non-empty matchings for a regular
--   expression in a given word. A matching is a pair of two numbers,
--   where the first is the index (>= 0) where the matched subword
--   starts and the second is the length (>= 1) of the matched
--   subword.
-- 
--   Not only the longest but all (non-empty) matchings are returned
--   in a specific order. 'allMatchings' are sorted by the sum of
--   'matchingIndex' and 'matchingLength' where smaller indices
--   precede larger indices if the corresponding sums with the length
--   are equal.
-- 
allMatchings :: RegExp (Set Int) a -> [a] -> [Matching]
allMatchings r = collect . process r . zip (map singleton [0..])
 where
  collect = concatMap matching . zip [0..]

  matching (end,s) = do start <- toList (activeLabel s)
                        return $ Matching start (end-start)

leftmostLongestMatching :: RegExp (Min Int) a -> [a] -> Maybe Matching
leftmostLongestMatching r = searchLL . process r . zip (map (Min . Just) [0..])
 where
  searchLL = foldl matching Nothing . zip [0..]

  matching Nothing  (end,s) = do start <- getMin $ activeLabel s
                                 return $ Matching start (end-start)
  matching (Just m) (end,s) =
    do start <- getMin $ activeLabel s
       guard (start < matchingIndex m || (start == matchingIndex m &&
                                          end-start > matchingLength m))
       return $ Matching start (end-start)
   `mplus` return m

newtype Min a = Min { getMin :: Maybe a }

instance Ord a => Monoid (Min a)
 where
  mempty      = Min Nothing
  mappend m n = case getMin m of
                  Nothing -> n
                  Just x  -> case getMin n of
                               Nothing -> m
                               Just y  -> Min (Just (min x y))

-- | Flipped version of 'leftmostLongestMatching' specialised for
--   strings. Useful in combination with the 'OverloadedStrings'
--   language extension to use string literals as regular expressions.
-- 
(=~) :: String -> RegExp (Min Int) Char -> Maybe Matching
(=~) = flip leftmostLongestMatching

process :: Monoid m => RegExp m a -> [(m,a)] -> [RegExp m a]
process r = scanl next r

next :: Monoid m => RegExp m a -> (m,a) -> RegExp m a
next x (m,a) = activateFirst a m (step x)
 where
  step y | isActive y = shift (unlabeled y)
         | otherwise  = y

  shift Epsilon      = epsilon
  shift (Symbol s p) = symbol s p
  shift (Star r)     = case status r of
                         Nothing -> star (step r)
                         Just n  -> star (activateFirst a n (step r))
  shift (r :*: s)    = case status r of
                         Nothing -> step r .*. step s
                         Just n  -> step r .*. activateFirst a n (step s)
  shift (r :+: s)    = step r .+. step s

activateFirst :: Monoid m => a -> m -> RegExp m a -> RegExp m a
activateFirst a m x =
  case unlabeled x of
    Epsilon                -> epsilon
    Symbol s p | p a       -> let n = mappend m (activeLabel x)
                               in Labeled False (Just n) (Symbol s p)
               | otherwise -> Labeled False Nothing (Symbol s p)
    Star r                 -> star (activateFirst a m r)
    r :*: s    | isEmpty r -> activateFirst a m r .*. activateFirst a m s
               | otherwise -> activateFirst a m r .*. s
    r :+: s                -> activateFirst a m r .+. activateFirst a m s
