{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Text.RegExp.Data where

import Data.Maybe
import Data.Monoid

-- | Regular expressions are represented as values of type 'RegExp'
--   @m@ @a@ and can be matched against lists of type @[a]@. Usually,
--   regular expressions of type 'RegExp' @m@ 'Char' are used to match
--   strings but particular applications may match against things
--   other than characters.
-- 
type RegExp m a = Labeled m (RE m a)

data Labeled m a = Labeled {
  isEmpty   :: Bool,
  status    :: Maybe m,
  unlabeled :: a }

data RE m a = Epsilon
            | Symbol String (a -> Bool)
            | Star (RegExp m a)
            | RegExp m a :*: RegExp m a
            | RegExp m a :+: RegExp m a

isActive :: RegExp m a -> Bool
isActive = isJust . status

label :: Monoid m => Maybe m -> m
label = fromMaybe mempty

activeLabel :: Monoid m => RegExp m a -> m
activeLabel = label . status

-- smart constructors

-- | Matches the empty word. 'epsilon' has no direct string
--   representation but is used to implement other constructs such as
--   'optional' components like @a?@.
-- 
epsilon :: RegExp m a
epsilon = Labeled True Nothing Epsilon

-- | Matches the given character.
-- 
char :: Char -> RegExp m Char
char c = symbol [c] (c==)

-- | Matches a symbol that satisfies the given predicate. The first
--   argument is used when printing regular expressions but is
--   irrelevant for the matching algorithm.
-- 
symbol :: String -> (a -> Bool) -> RegExp m a
symbol s = Labeled False Nothing . Symbol s

-- | Matches zero or more occurrences of the given regular
--   expression. For example @a*@ matches the character @a@ zero or
--   more times.
-- 
star :: RegExp m a -> RegExp m a
star r@(Labeled _ s _) = Labeled True s (Star r)

infixr 7 :*:, .*.

-- | Matches two regular expressions in sequence. In their string
--   representation sequences of regular expressions are just written,
--   well, in sequence like in @a?b*c@.
-- 
(.*.) :: Monoid m => RegExp m a -> RegExp m a -> RegExp m a
r@(Labeled d k _) .*. s@(Labeled e l _) = Labeled (d&&e) (status k l) (r :*: s)
 where
  status Nothing Nothing     = Nothing
  status _        _      | e = mappend k l
  status _        _          = Just (label l)

infixr 6 :+:, .+.

-- | Matches any of the given regular expressions. For example @a|b@
--   matches either the character @a@ or @b@.
-- 
(.+.) :: Monoid m => RegExp m a -> RegExp m a -> RegExp m a
r@(Labeled d k _) .+. s@(Labeled e l _) = Labeled (d||e) (mappend k l) (r :+: s)

-- | Matches one or more occurrences of the given regular
--   expression. For example @a+@ matches the character @a@ one or
--   more times.
-- 
plus :: Monoid m => RegExp m a -> RegExp m a
plus r = r .*. star r

-- | Matches the given regular expression or the empty
--   word. 'optional' components are usually written @a?@ but could
--   also be written @(|a)@, that is, as alternative between 'epsilon'
--   and @a@.
-- 
optional :: Monoid m => RegExp m a -> RegExp m a
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
bounded :: Monoid m => RegExp m a -> (Int,Int) -> RegExp m a
bounded r (n,m) =
  foldr (.*.) (foldr (.*.) epsilon (replicate (m-n) (optional r)))
              (replicate n r)

-- pretty printing

instance Show (RegExp m Char)
 where
  showsPrec p x =
   case unlabeled x of
    Epsilon    -> id
    Symbol _ _ -> showString (showSymbol x)
    Star r     -> showsPrec 3 r . showString "*"
    r :*: s    -> showParen (p>2) (showsPrec 2 r.showsPrec 2 s)
    r :+: s    -> showParen (p>1) (showsPrec 1 r.showString "|".showsPrec 1 s)

showSymbol :: RegExp m Char -> String
showSymbol r | isActive r = "\ESC[91m" ++ s ++ "\ESC[0m"
             | otherwise  = s
 where Symbol s _ = unlabeled r
