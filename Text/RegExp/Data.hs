{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Text.RegExp.Data where

data RE s a = Epsilon
            | Symbol a
            | Star (s (RE s a))
            | s (RE s a) :*: s (RE s a)
            | s (RE s a) :+: s (RE s a)

type RegExp a = Labeled (RE Labeled a)

data Labeled a = Labeled Bool Status a

unlabeled :: Labeled a -> a
unlabeled (Labeled _ _ a) = a

data Status = Inactive | Active [Index]
 deriving Eq

type Index = Int

isEmpty :: RegExp a -> Bool
isEmpty (Labeled e _ _) = e

status :: RegExp a -> Status
status (Labeled _ s _) = s

isActive :: RegExp a -> Bool
isActive = (Inactive/=) . status

finalIndices :: RegExp a -> [Index]
finalIndices = indices . status

indices :: Status -> [Index]
indices Inactive    = []
indices (Active is) = is

mergeStatus :: Status -> Status -> Status
mergeStatus Inactive    y           = y
mergeStatus x           Inactive    = x
mergeStatus (Active is) (Active js) = Active (mergeIndices is js)

mergeIndices :: [Index] -> [Index] -> [Index]
mergeIndices []         js         = js
mergeIndices is         []         = is
mergeIndices is@(i:is') js@(j:js') = case compare i j of
                                       LT -> i : mergeIndices is' js
                                       EQ -> i : mergeIndices is' js'
                                       GT -> j : mergeIndices is js'

-- smart constructors

epsilon :: RegExp a
epsilon = Labeled True Inactive Epsilon

symbol :: a -> RegExp a
symbol a = Labeled False Inactive (Symbol a)

star :: RegExp a -> RegExp a
star r@(Labeled _ s _) = Labeled True s (Star r)

infixr 7 :*:, .*.

(.*.) :: RegExp a -> RegExp a -> RegExp a
r@(Labeled d k _) .*. s@(Labeled e l _) = Labeled (d&&e) (status k l) (r :*: s)
 where
  status Inactive Inactive     = Inactive
  status _        _        | e = Active (mergeIndices (indices k) (indices l))
  status _        _            = Active (indices l)

infixr 6 :+:, .+.

(.+.) :: RegExp a -> RegExp a -> RegExp a
r@(Labeled d k _) .+. s@(Labeled e l _) =
  Labeled (d||e) (mergeStatus k l) (r :+: s)

plus :: RegExp a -> RegExp a
plus r = r .*. star r

optional :: RegExp a -> RegExp a
optional r = epsilon .+. r

bounded :: RegExp a -> (Int,Int) -> RegExp a
bounded r (n,m) =
  foldr (.*.) (foldr (.*.) epsilon (replicate (m-n) (optional r)))
              (replicate n r)

-- pretty printing

instance Show (RegExp Char)
 where
  showsPrec p x =
   case unlabeled x of
    Epsilon  -> id
    Symbol _ -> showString (showSymbol x)
    Star r   -> showsPrec 3 r . showString "*"
    r:*:s    -> showParen (p>2) (showsPrec 2 r . showsPrec 2 s)
    r:+:s    -> showParen (p>1) (showsPrec 1 r . showString "|" . showsPrec 1 s)

showSymbol :: RegExp Char -> String
showSymbol r | isActive r = "\ESC[91m" ++ a : "\ESC[0m"
             | otherwise  = [a]
 where Symbol a = unlabeled r
