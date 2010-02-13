{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Text.RegExp.Data where

data RE s a = Epsilon
            | Symbol a
            | Star (s (RE s a))
            | s (RE s a) :*: s (RE s a)
            | s (RE s a) :+: s (RE s a)

type RegExp a = Labeled (RE Labeled a)

data Labeled a = Labeled Bool Status a

data Status = Inactive | Active Bool
 deriving (Eq,Ord)

unlabeled :: Labeled a -> a
unlabeled (Labeled _ _ a) = a

-- smart constructors

epsilon :: RegExp a
epsilon = Labeled True Inactive Epsilon

symbol :: a -> RegExp a
symbol a = Labeled False Inactive (Symbol a)

star :: RegExp a -> RegExp a
star r@(Labeled _ s _) = Labeled True s (Star r)

infixr 7 :*:, .*.

(.*.) :: RegExp a -> RegExp a -> RegExp a
r@(Labeled d k _) .*. s@(Labeled e l _) = Labeled (d&&e) (status k l) (r:*:s)
 where
  status _          (Active True) = Active True
  status (Active a) _             = Active (a&&e)
  status Inactive   b             = b

infixr 6 :+:, .+.

(.+.) :: RegExp a -> RegExp a -> RegExp a
r@(Labeled d k _) .+. s@(Labeled e l _) = Labeled (d||e) (max k l) (r:+:s)

plus :: RegExp a -> RegExp a
plus r = r .*. star r

optional :: RegExp a -> RegExp a
optional r = epsilon .+. r

bounded :: RegExp a -> (Int,Int) -> RegExp a
bounded r (n,m) =
  foldr (.*.) (foldr (.*.) epsilon (replicate (m-n) (optional r)))
              (replicate n r)

-- auxiliary functions

isEmpty :: RegExp a -> Bool
isEmpty (Labeled e _ _) = e

isActive :: RegExp a -> Bool
isActive (Labeled _ (Active _) _) = True
isActive _                        = False

isFinal :: RegExp a -> Bool
isFinal (Labeled _ (Active a) _) = a
isFinal _                        = False

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

-- library function

scan :: (a -> b -> Maybe a) -> a -> [b] -> [a]
scan _ x []     = [x]
scan f x (y:ys) = x : maybe [] (\z -> scan f z ys) (f x y)

