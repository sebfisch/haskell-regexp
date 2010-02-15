module Text.RegExp.Matcher where

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

-- | Checks whether a regular expression matches (a subword of) the
--   given word. For example, @accept (fromString \"b|abc\") \"ab\"@
--   yields @True@ because the second character in the given string
--   can be matched against @b@.
-- 
accept :: RegExp a -> [a] -> Bool
accept r xs = isEmpty r || (not . null . matchings r $ xs)

-- | Returns a list of non-empty matchings for a regular expression in
--   a given word. A matching is a pair of two numbers, where the
--   first is the index (>= 0) where the matched subword starts and
--   the second is the length (>= 1) of the matched subword.
-- 
--   Not only the longest but all (non-empty) matchings are returned
--   in a specific order. The 'matchings' are sorted by the sum of
--   'matchingIndex' and 'matchingLength' where smaller indices
--   precede larger indices if the corresponding sums with the length
--   are equal.
-- 
matchings :: RegExp a -> [a] -> [Matching]
matchings r = concatMap matching . zip [0..] . process r
 where
  matching (end,s) = map (\start -> Matching start (end-start)) (finalIndices s)

-- | Flipped version of 'matchings' specialised for strings. Useful in
--   combination with the 'OverloadedStrings' language extension to
--   use string literals as regular expressions. For example, the call
--   @\"abc\" =~ \"b|abc|c\"@ yields @[(1,1),(0,3),(2,1)]@.
-- 
(=~) :: String -> RegExp Char -> [Matching]
(=~) = flip matchings

process :: RegExp a -> [a] -> [RegExp a]
process r = scanl next r . zip [0..]

next :: RegExp a -> (Int,a) -> RegExp a
next x (i,a) = activateFirst a [i] (step x)
 where
  step y | isActive y = shift (unlabeled y)
         | otherwise  = y

  shift Epsilon      = epsilon
  shift (Symbol s p) = symbol s p
  shift (Star r)     = activateFirst a (finalIndices r) (star (step r))
  shift (r :*: s)    = step r .*. activateFirst a (finalIndices r) (step s)
  shift (r :+: s)    = step r .+. step s

activateFirst :: a -> [Int] -> RegExp a -> RegExp a
activateFirst _ [] x = x
activateFirst a is x =
  case unlabeled x of
    Epsilon                -> epsilon
    Symbol s p | p a       -> let js = mergeIndices is (finalIndices x)
                               in Labeled False (Active js) (Symbol s p)
               | otherwise -> Labeled False Inactive (Symbol s p)
    Star r                 -> star (activateFirst a is r)
    r :*: s    | isEmpty r -> activateFirst a is r .*. activateFirst a is s
               | otherwise -> activateFirst a is r .*. s
    r :+: s                -> activateFirst a is r .+. activateFirst a is s
