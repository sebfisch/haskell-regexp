{-# LANGUAGE FlexibleInstances #-}

module Text.RegExp.Data where

import Data.Maybe
import Data.Semiring

-- | Regular expressions are represented as values of type 'RegExp'
--   @w@ @a@ and can be matched against lists of type @[a]@. Usually,
--   regular expressions of type 'RegExp' @w@ 'Char' are used to match
--   strings but particular applications may match against things
--   other than characters.
-- 
data RegExp w a = RegExp { empty :: !w, final :: !w, regExp :: RE w a }

data RE w a = Weight w
            | Symbol String (a -> Bool)
            | Star (RegExp w a)
            | RegExp w a :+: RegExp w a
            | RegExp w a :*: RegExp w a

isSymbol :: RegExp s a -> Bool
isSymbol r = case regExp r of
               Symbol _ _ -> True
               _          -> False

weightSymbols :: Semiring w => w -> RegExp w a -> RegExp w a
weightSymbols w x | isSymbol x = weight w .*. x
                  | otherwise  = attach (regExp x)
 where
  attach (Weight w) = weight w
  attach (Star r)   = star (weightSymbols w r)
  attach (r :+: s)  = weightSymbols w r .+. weightSymbols w s
  attach (r :*: s)  = weightSymbols w r .*. weightSymbols w s

-- smart constructors

-- | Matches the empty word. 'epsilon' has no direct string
--   representation but is used to implement other constructs such as
--   'optional' components like @a?@.
-- 
epsilon :: Semiring w => RegExp w a
epsilon = weight one

-- | Matches the empty word. 'weight' is like 'epsilon' but has a
--   specific user-defined weight. Weights can be elements of any
--   'Semiring'.
-- 
weight :: CommutativeMonoid w => w -> RegExp w a
weight w = RegExp w zero (Weight w)

-- | Matches the given character.
-- 
char :: CommutativeMonoid w => Char -> RegExp w Char
char c = symbol [c] (c==)

-- | Matches a symbol that satisfies the given predicate. The first
--   argument is used when printing regular expressions but is
--   irrelevant for the matching algorithm.
-- 
symbol :: CommutativeMonoid w => String -> (a -> Bool) -> RegExp w a
symbol s = RegExp zero zero . Symbol s

-- | Matches an arbitrary symbol.
-- 
anySymbol :: Semiring s => RegExp s a
anySymbol = symbol "." (const True)

-- | Matches zero or more occurrences of the given regular
--   expression. For example @a*@ matches the character @a@ zero or
--   more times.
-- 
star :: Semiring w => RegExp w a -> RegExp w a
star r@(RegExp _ w _) = RegExp one w (Star r)

instance CommutativeMonoid w => CommutativeMonoid (RegExp w a)
 where
  zero = weight zero
  r@(RegExp d v _) .+. s@(RegExp e w _) = RegExp (d.+.e) (v.+.w) (r:+:s)

instance Semiring w => Semiring (RegExp w a)
 where
  one = epsilon
  r@(RegExp d v _) .*. s@(RegExp e w _) = RegExp (d.*.e) (v.*.e.+.w) (r:*:s)

-- | Matches one or more occurrences of the given regular
--   expression. For example @a+@ matches the character @a@ one or
--   more times.
-- 
plus :: Semiring w => RegExp w a -> RegExp w a
plus r = r .*. star r

-- | Matches the given regular expression or the empty
--   word. 'optional' components are usually written @a?@ but could
--   also be written @(|a)@, that is, as alternative between 'epsilon'
--   and @a@.
-- 
optional :: Semiring w => RegExp w a -> RegExp w a
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
bounded :: Semiring w => RegExp w a -> (Int,Int) -> RegExp w a
bounded r (n,m) =
  foldr (.*.) (foldr (.*.) epsilon (replicate (m-n) (optional r)))
              (replicate n r)

-- pretty printing

instance (Eq w, Show w, CommutativeMonoid w) => Show (RegExp w Char)
 where
  showsPrec p x =
   case regExp x of
    Weight w   -> shows w
    Symbol _ _ -> showString (showSymbol x)
    Star r     -> showsPrec 3 r . showString "*"
    r :*: s    -> showParen (p>2) (showsPrec 2 r.showsPrec 2 s)
    r :+: s    -> showParen (p>1) (showsPrec 1 r.showString "|".showsPrec 1 s)

showSymbol :: (Eq w, CommutativeMonoid w) => RegExp w Char -> String
showSymbol r | final r /= zero = "\ESC[91m" ++ s ++ "\ESC[0m"
             | otherwise       = s
 where Symbol s _ = regExp r
