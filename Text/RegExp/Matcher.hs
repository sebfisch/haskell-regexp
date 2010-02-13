module Text.RegExp.Matcher where

import Text.RegExp.Data

accept :: RegExp a -> [a] -> Bool
accept r = not . null . matchings r

matchings :: RegExp a -> [a] -> [(Index,Int)]
matchings r = concatMap matching . zip [0..] . process r
 where
  matching (end,s) = map (\start -> (start,end-start)) (finalIndices s)

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
