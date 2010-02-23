{-# LANGUAGE FlexibleInstances #-}

module Text.RegExp.Simple where

data RegExp a = RegExp { empty :: !Bool, final :: !Bool, regExp :: RE a }

data RE a = Epsilon
          | Symbol String (a -> Bool)
          | Star (RegExp a)
          | RegExp a :*: RegExp a
          | RegExp a :+: RegExp a

-- smart constructors

epsilon :: RegExp a
epsilon = RegExp True False Epsilon

char :: Char -> RegExp Char
char c = symbol [c] (c==)

symbol :: String -> (a -> Bool) -> RegExp a
symbol s p = RegExp False False (Symbol s p)

anySymbol :: RegExp a
anySymbol = symbol "." (const True)

star :: RegExp a -> RegExp a
star (RegExp e s r) = RegExp True s (Star (RegExp e s r))

infixr 6 .+.

(.+.) :: RegExp a -> RegExp a -> RegExp a
(RegExp d v r) .+. (RegExp e w s) =
  RegExp (d||e) (v||w) (RegExp d v r :+: RegExp e w s)

infixr 7 .*.

(.*.) :: RegExp a -> RegExp a -> RegExp a
(RegExp d v r) .*. (RegExp e w s) =
  RegExp (d&&e) (v&&e || w) (RegExp d v r :*: RegExp e w s)

plus :: RegExp a -> RegExp a
plus r = r .*. star r

optional :: RegExp a -> RegExp a
optional r = epsilon .+. r

bounded :: RegExp a -> (Int,Int) -> RegExp a
bounded _ (0,0) = epsilon
bounded r (0,m) = optional r .*. bounded r (0,m-1)
bounded r (n,m) = r .*. bounded r (n-1,m-1)

-- matching

accept :: RegExp a -> [a] -> Bool
accept r []     = empty r
accept r (x:xs) = final (go (next True r x) xs)
 where
  go s []     = s
  go s (y:ys) = go (next False s y) ys

next :: Bool -> RegExp a -> a -> RegExp a
next t x a = pass (regExp x)
 where
  pass Epsilon      = epsilon
  pass (Symbol s p) = RegExp False (if p a then t else False) (Symbol s p)
  pass (Star r)     = star (next (t || final r) r a)
  pass (r :*: s)    = next t r a .*. next (t && empty r || final r) s a
  pass (r :+: s)    = next t r a .+. next t s a

instance Show (RegExp Char)
 where
  showsPrec p x =
   case regExp x of
    Epsilon    -> id
    Symbol _ _ -> showString (showSymbol x)
    Star r     -> showsPrec 3 r . showString "*"
    r :*: s    -> showParen (p>2) (showsPrec 2 r.showsPrec 2 s)
    r :+: s    -> showParen (p>1) (showsPrec 1 r.showString "|".showsPrec 1 s)

showSymbol :: RegExp Char -> String
showSymbol r | final r   = "\ESC[91m" ++ s ++ "\ESC[0m"
             | otherwise = s
 where Symbol s _ = regExp r
