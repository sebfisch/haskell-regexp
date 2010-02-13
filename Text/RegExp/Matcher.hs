module Text.RegExp.Matcher where

import Text.RegExp.Data

accept :: Eq a => RegExp a -> [a] -> Bool
accept r = any isFinal . process r

process :: Eq a => RegExp a -> [a] -> [RegExp a]
process = scanl next

next :: Eq a => RegExp a -> a -> RegExp a
next x a = activateFirst a (step x)
 where
  step y | isActive y = shift (unlabeled y)
         | otherwise  = y

  shift Epsilon                = epsilon
  shift (Symbol b)             = symbol b
  shift (Star r)   | isFinal r = activateFirst a (star (step r))
                   | otherwise = star (step r)
  shift (r :*: s)  | isFinal r = step r .*. activateFirst a (step s)
                   | otherwise = step r .*. step s
  shift (r :+: s)              = step r .+. step s

activateFirst :: Eq a => a -> RegExp a -> RegExp a
activateFirst a x =
  case unlabeled x of
    Epsilon              -> epsilon
    Symbol b | a==b      -> Labeled False (Active True) (Symbol b)
             | otherwise -> Labeled False Inactive (Symbol b)
    Star r               -> star (activateFirst a r)
    r :*: s  | isEmpty r -> activateFirst a r .*. activateFirst a s
             | otherwise -> activateFirst a r .*. s
    r :+: s              -> activateFirst a r .+. activateFirst a s
