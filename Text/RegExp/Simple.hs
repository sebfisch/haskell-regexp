module Text.RegExp.Simple where

data RegExp a = RegExp { isEmpty :: Bool, status :: Maybe Bool, regExp :: RE a }

data RE a = Epsilon
          | Symbol (a -> Bool)
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
char c = symbol (c==)

symbol :: (a -> Bool) -> RegExp a
symbol p = RegExp False Nothing (Symbol p)

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
next x a = activateFirst a True (step x)
 where
  step y | isActive y = shift (regExp y)
         | otherwise  = y

  shift Epsilon    = epsilon
  shift (Symbol p) = symbol p
  shift (Star r)   = star (activateFirst a (isFinal r) (step r))
  shift (r :*: s)  = step r .*. activateFirst a (isFinal r) (step s)
  shift (r :+: s)  = step r .+. step s

activateFirst :: a -> Bool -> RegExp a -> RegExp a
activateFirst _ False x = x
activateFirst a m     x =
  case regExp x of
    Epsilon              -> epsilon
    Symbol p | p a       -> RegExp False (Just m) (Symbol p)
             | otherwise -> RegExp False Nothing (Symbol p)
    Star r               -> star (activateFirst a m r)
    r :*: s  | isEmpty r -> activateFirst a m r .*. activateFirst a m s
             | otherwise -> activateFirst a m r .*. s
    r :+: s              -> activateFirst a m r .+. activateFirst a m s
