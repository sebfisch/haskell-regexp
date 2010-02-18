{-# LANGUAGE FlexibleInstances #-}

module Text.RegExp.Simple where

data RegExp a = RegExp { isEmpty :: Bool, status :: Maybe Bool, regExp :: RE a }

data RE a = Epsilon
          | Symbol String (a -> Bool)
          | Star (RegExp a)
          | RegExp a :*: RegExp a
          | RegExp a :+: RegExp a
 
isActive :: RegExp a -> Bool
isActive r = case status r of
               Nothing -> False
               Just _  -> True
 
final :: Maybe Bool -> Bool
final Nothing  = False
final (Just b) = b
 
isFinal :: RegExp a -> Bool
isFinal r = final (status r)
 
maybeOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
maybeOr Nothing  b        = b
maybeOr a        Nothing  = a
maybeOr (Just x) (Just y) = Just (x||y)
 
-- smart constructors
 
epsilon :: RegExp a
epsilon = RegExp True Nothing Epsilon
 
char :: Char -> RegExp Char
char c = symbol [c] (c==)
 
symbol :: String -> (a -> Bool) -> RegExp a
symbol s p = RegExp False Nothing (Symbol s p)
 
star :: RegExp a -> RegExp a
star (RegExp e s r) = RegExp True s (Star (RegExp e s r))
 
infixr 7 .*.
 
(.*.) :: RegExp a -> RegExp a -> RegExp a
(RegExp d k r) .*. (RegExp e l s) =
  RegExp (d&&e) (status k l) (RegExp d k r :*: RegExp e l s)
 where
  status Nothing Nothing     = Nothing
  status _        _      | e = maybeOr k l
  status _        _          = Just (final l)
 
infixr 6 .+.
 
(.+.) :: RegExp a -> RegExp a -> RegExp a
(RegExp d k r) .+. (RegExp e l s) =
  RegExp (d||e) (maybeOr k l) (RegExp d k r :+: RegExp e l s)

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
accept r xs = isEmpty r || go r xs
 where
  go s []     = isFinal s
  go s (x:xs) = isFinal s || go (next s x) xs

next :: RegExp a -> a -> RegExp a
next x a = step True x
 where
  step b y | b || isActive y = shift b (regExp y)
           | otherwise       = y

  shift b Epsilon      = epsilon
  shift b (Symbol s p) = RegExp False
                                (if b && p a then Just True else Nothing)
                                (Symbol s p)
  shift b (Star r)     = star (step (b || isFinal r) r)
  shift b (r :*: s)    = step b r .*. step (b && isEmpty r || isFinal r) s
  shift b (r :+: s)    = step b r .+. step b s


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
showSymbol r | isActive r = "\ESC[91m" ++ s ++ "\ESC[0m"
             | otherwise  = s
 where Symbol s _ = regExp r
