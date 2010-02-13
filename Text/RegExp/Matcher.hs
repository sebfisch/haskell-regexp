module Text.RegExp.Matcher where

import Text.RegExp.Data

accept :: Eq a => RegExp a -> [a] -> Bool
accept r = not . null . matchings r

matchings :: Eq a => RegExp a -> [a] -> [(Index,Int)]
matchings r = concatMap matching . zip [0..] . process r
 where
  matching (end,s) = map (\start -> (start,end-start)) (finalIndices s)

(=~) :: RegExp Char -> String -> [(Index,Int)]
(=~) = matchings

process :: Eq a => RegExp a -> [a] -> [RegExp a]
process r = scanl next r . zip [0..]

next :: Eq a => RegExp a -> (Index,a) -> RegExp a
next x (i,a) = activateFirst a [i] (step x)
 where
  step y | isActive y = shift (unlabeled y)
         | otherwise  = y

  shift Epsilon    = epsilon
  shift (Symbol b) = symbol b
  shift (Star r)   = activateFirst a (finalIndices r) (star (step r))
  shift (r :*: s)  = step r .*. activateFirst a (finalIndices r) (step s)
  shift (r :+: s)  = step r .+. step s

activateFirst :: Eq a => a -> [Index] -> RegExp a -> RegExp a
activateFirst _ [] x = x
activateFirst a is x =
  case unlabeled x of
    Epsilon              -> epsilon
    Symbol b | a==b      -> let js = mergeIndices is (finalIndices x)
                             in Labeled False (Active js) (Symbol b)
             | otherwise -> Labeled False Inactive (Symbol b)
    Star r               -> star (activateFirst a is r)
    r :*: s  | isEmpty r -> activateFirst a is r .*. activateFirst a is s
             | otherwise -> activateFirst a is r .*. s
    r :+: s              -> activateFirst a is r .+. activateFirst a is s
