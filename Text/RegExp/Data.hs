{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Text.RegExp.Data where

-- | Regular expressions are represented as values of type 'RegExp'
--   @a@ and can be matched against lists of type @[a]@. Usually,
--   regular expressions of type 'RegExp' 'Char' are used to match
--   strings but particular applications may match against things
--   other than characters.
-- 
type RegExp a = Labeled (RE Labeled a)

data RE l a = Epsilon
            | Symbol String (a -> Bool)
            | Star (l (RE l a))
            | l (RE l a) :*: l (RE l a)
            | l (RE l a) :+: l (RE l a)

data Labeled a = Labeled Bool Status a

unlabeled :: Labeled a -> a
unlabeled (Labeled _ _ a) = a

data Status = Inactive | Active [Int]
 deriving Eq

isEmpty :: RegExp a -> Bool
isEmpty (Labeled e _ _) = e

status :: RegExp a -> Status
status (Labeled _ s _) = s

isActive :: RegExp a -> Bool
isActive = (Inactive/=) . status

finalIndices :: RegExp a -> [Int]
finalIndices = indices . status

indices :: Status -> [Int]
indices Inactive    = []
indices (Active is) = is

mergeStatus :: Status -> Status -> Status
mergeStatus Inactive    y           = y
mergeStatus x           Inactive    = x
mergeStatus (Active is) (Active js) = Active (mergeIndices is js)

mergeIndices :: [Int] -> [Int] -> [Int]
mergeIndices []         js         = js
mergeIndices is         []         = is
mergeIndices is@(i:is') js@(j:js') = case compare i j of
                                       LT -> i : mergeIndices is' js
                                       EQ -> i : mergeIndices is' js'
                                       GT -> j : mergeIndices is js'

-- smart constructors

-- | Matches the empty word. 'epsilon' has no direct string
--   representation but is used to implement other constructs such as
--   'optional' components like @a?@.
-- 
epsilon :: RegExp a
epsilon = Labeled True Inactive Epsilon

-- | Matches the given character.
-- 
char :: Char -> RegExp Char
char c = symbol [c] (c==)

-- | Matches a symbol that satisfies the given predicate. The first
--   argument is used when printing regular expressions but is
--   irrelevant for the matching algorithm.
-- 
symbol :: String -> (a -> Bool) -> RegExp a
symbol s = Labeled False Inactive . Symbol s

-- | Matches zero or more occurrences of the given regular
--   expression. For example @a*@ matches the character @a@ zero or
--   more times.
-- 
star :: RegExp a -> RegExp a
star r@(Labeled _ s _) = Labeled True s (Star r)

infixr 7 :*:, .*.

-- | Matches two regular expressions in sequence. In their string
--   representation sequences of regular expressions are just written,
--   well, in sequence like in @a?b*c@.
-- 
(.*.) :: RegExp a -> RegExp a -> RegExp a
r@(Labeled d k _) .*. s@(Labeled e l _) = Labeled (d&&e) (status k l) (r :*: s)
 where
  status Inactive Inactive     = Inactive
  status _        _        | e = Active (mergeIndices (indices k) (indices l))
  status _        _            = Active (indices l)

infixr 6 :+:, .+.

-- | Matches any of the given regular expressions. For example @a|b@
--   matches either the character @a@ or @b@.
-- 
(.+.) :: RegExp a -> RegExp a -> RegExp a
r@(Labeled d k _) .+. s@(Labeled e l _) =
  Labeled (d||e) (mergeStatus k l) (r :+: s)

-- | Matches one or more occurrences of the given regular
--   expression. For example @a+@ matches the character @a@ one or
--   more times.
-- 
plus :: RegExp a -> RegExp a
plus r = r .*. star r

-- | Matches the given regular expression or the empty
--   word. 'optional' components are usually written @a?@ but could
--   also be written @(|a)@, that is, as alternative between 'epsilon'
--   and @a@.
-- 
optional :: RegExp a -> RegExp a
optional r = epsilon .+. r

-- | Matches a regular expression a given number of times. For
--   example, the regular expression @a{4,7}@ matches the character
--   @a@ four to seven times. If the minimal and maximal occurences
--   are identical, one can be left out, that is, @a{2}@ matches two
--   occurrences of the character @a@.
-- 
--   Numerical bounds are implemented via translation into ordinary
--   regular expressions. For example, @a{4,7}@ is translated into
--   @aaaaa?a?a?@.
-- 
bounded :: RegExp a -> (Int,Int) -> RegExp a
bounded r (n,m) =
  foldr (.*.) (foldr (.*.) epsilon (replicate (m-n) (optional r)))
              (replicate n r)

-- pretty printing

instance Show (RegExp Char)
 where
  showsPrec p x =
   case unlabeled x of
    Epsilon    -> id
    Symbol _ _ -> showString (showSymbol x)
    Star r     -> showsPrec 3 r . showString "*"
    r :*: s    -> showParen (p>2) (showsPrec 2 r.showsPrec 2 s)
    r :+: s    -> showParen (p>1) (showsPrec 1 r.showString "|".showsPrec 1 s)

showSymbol :: RegExp Char -> String
showSymbol r | isActive r = "\ESC[91m" ++ s ++ "\ESC[0m"
             | otherwise  = s
 where Symbol s _ = unlabeled r
