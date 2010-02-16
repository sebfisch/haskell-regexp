module Text.RegExp.Simple where

import Data.Maybe

data RegExp a = RegExp { isEmpty :: Bool, status :: Maybe Bool, regExp :: RE a }

data RE a = Epsilon
          | Symbol (a -> Bool)
          | Star (RegExp a)
          | RegExp a :*: RegExp a
          | RegExp a :+: RegExp a

isActive :: RegExp a -> Bool
isActive = isJust . status

final :: Maybe Bool -> Bool
final = fromMaybe False

isFinal :: RegExp a -> Bool
isFinal = final . status

maybeOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
maybeOr Nothing  b        = b
maybeOr a        Nothing  = a
maybeOr (Just x) (Just y) = Just (x||y)

-- smart constructors

epsilon :: RegExp a
epsilon = RegExp True Nothing Epsilon

char :: Char -> RegExp Char
char = symbol . (==)

symbol :: (a -> Bool) -> RegExp a
symbol = RegExp False Nothing . Symbol

star :: RegExp a -> RegExp a
star r@(RegExp _ s _) = RegExp True s (Star r)

infixr 7 .*.

(.*.) :: RegExp a -> RegExp a -> RegExp a
r@(RegExp d k _) .*. s@(RegExp e l _) = RegExp (d&&e) (status k l) (r :*: s)
 where
  status Nothing Nothing     = Nothing
  status _        _      | e = maybeOr k l
  status _        _          = Just (final l)

infixr 6 .+.

(.+.) :: RegExp a -> RegExp a -> RegExp a
r@(RegExp d k _) .+. s@(RegExp e l _) = RegExp (d||e) (maybeOr k l) (r :+: s)

plus :: RegExp a -> RegExp a
plus r = r .*. star r

optional :: RegExp a -> RegExp a
optional r = epsilon .+. r

bounded :: RegExp a -> (Int,Int) -> RegExp a
bounded r (n,m) =
  foldr (.*.) (foldr (.*.) epsilon (replicate (m-n) (optional r)))
              (replicate n r)

-- matching

accept :: RegExp a -> [a] -> Bool
accept r xs = isEmpty r || any isFinal (scanl next r xs)

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
