module Text.RegExp.Matcher where

import Text.RegExp.Data

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
--   in a specific order. The list returned by 'matchings' is sorted
--   by the sum of index and length where smaller indices precede
--   larger indices if the corresponding sums with the length are
--   equal. For example, the call 
--   @matchings (fromString \"b|abc|c\") \"abc\"@ yields
--   @[(1,1),(0,3),(2,1)]@. The first matching
--   @(1,1)@ is the chararacter @b@, the second the complete word
--   @abc@ and the third is the character @c@. The @b@ is returned
--   first because it ends first and @abc@ is returned before @c@
--   because they both end at the same position but @abc@ starts
--   earlier.
-- 
matchings :: RegExp a -> [a] -> [(Index,Int)]
matchings r = concatMap matching . zip [0..] . process r
 where
  matching (end,s) = map (\start -> (start,end-start)) (finalIndices s)

-- | Flipped version of 'matchings' specialised for strings. Useful in
--   combination with the 'OverloadedStrings' language extension to
--   use string literals as regular expressions. For example, the call
--   @\"abc\" =~ \"b|abc|c\"@ yields @[(1,1),(0,3),(2,1)]@.
-- 
(=~) :: String -> RegExp Char -> [(Index,Int)]
(=~) = flip matchings

process :: RegExp a -> [a] -> [RegExp a]
process r = scanl next r . zip [0..]

next :: RegExp a -> (Index,a) -> RegExp a
next x (i,a) = activateFirst a [i] (step x)
 where
  step y | isActive y = shift (unlabeled y)
         | otherwise  = y

  shift Epsilon      = epsilon
  shift (Symbol s p) = symbol s p
  shift (Star r)     = activateFirst a (finalIndices r) (star (step r))
  shift (r :*: s)    = step r .*. activateFirst a (finalIndices r) (step s)
  shift (r :+: s)    = step r .+. step s

activateFirst :: a -> [Index] -> RegExp a -> RegExp a
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
