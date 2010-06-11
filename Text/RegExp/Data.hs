{-# LANGUAGE RankNTypes, MultiParamTypeClasses #-}

module Text.RegExp.Data where

import Data.Maybe
import Data.Semiring

import Prelude hiding ( seq )

-- |
-- Regular expressions are represented as values of type 'RegExp' @c@
-- where @c@ is the character type of the underlying alphabet. Values
-- of type @RegExp@ @c@ can be matched against lists of type @[c]@.
-- 
newtype RegExp c = RegExp (forall w . Semiring w => RegW w c)

data RegW w c = RegW { active :: !Bool,
                       empty  :: !w, 
                       final_ :: !w, 
                       reg    :: Reg w c }

final :: Semiring w => RegW w c -> w
final r = if active r then final_ r else zero

data Reg w c = Eps
             | Sym (c -> w)
             | Alt (RegW w c) (RegW w c)
             | Seq (RegW w c) (RegW w c)
             | Rep (RegW w c)

class Semiring w => Weight a b w where
  symWeight :: (a -> w) -> b -> w

weighted :: Weight a b w => RegW w a -> RegW w b
weighted (RegW a e f r) =
  case r of
    Eps     -> RegW a e f Eps
    Sym p   -> RegW a e f (Sym (symWeight p))
    Alt p q -> RegW a e f (Alt (weighted p) (weighted q))
    Seq p q -> RegW a e f (Seq (weighted p) (weighted q))
    Rep p   -> RegW a e f (Rep (weighted p))

-- |
-- Matches the empty word. 'eps' has no direct string representation
-- but is used to implement other constructs such as optional
-- components like @a?@.
-- 
eps :: RegExp c
eps = RegExp epsW

epsW :: Semiring w => RegW w c
epsW = RegW False one zero Eps

-- | Matches the given character.
-- 
sym :: Char -> RegExp Char
sym = psym . (==)

-- | Matches a symbol that satisfies the given predicate.
-- 
psym :: (c -> Bool) -> RegExp c
psym p = RegExp (symW (fromBool . p))

symW :: Semiring w => (c -> w) -> RegW w c
symW p = RegW False zero zero $ Sym p

-- | Matches an arbitrary symbol.
-- 
anySym :: RegExp c
anySym = psym (const True)

-- |
-- Matches either of two regular expressions. For example @a+b@
-- matches either the character @a@ or the character @b@.
-- 
alt :: RegExp c -> RegExp c -> RegExp c
alt (RegExp p) (RegExp q) =
  RegExp (RegW False (empty p .+. empty q) zero (Alt p q))

altW :: Semiring w => RegW w c -> RegW w c -> RegW w c
altW p q = RegW (active p || active q)
                (empty p .+. empty q)
                (final p .+. final q)
                (Alt p q)

-- |
-- Matches the sequence of two regular expressions. For example the
-- regular expressions @ab@ matches the word @ab@.
-- 
seq :: RegExp c -> RegExp c -> RegExp c
seq (RegExp p) (RegExp q) =
  RegExp (RegW False (empty p .*. empty q) zero (Seq p q))

seqW :: Semiring w => RegW w c -> RegW w c -> RegW w c
seqW p q = RegW (active p || active q)
                (empty p .*. empty q)
                (final p .*. empty q .+. final q)
                (Seq p q)

-- | Matches zero or more occurrences of the given regular
--   expression. For example @a*@ matches the character @a@ zero or
--   more times.
-- 
rep :: RegExp c -> RegExp c
rep (RegExp r) = RegExp (RegW False one zero (Rep r))

repW :: Semiring w => RegW w c -> RegW w c
repW r = RegW (active r) one (final r) (Rep r)

-- | Matches one or more occurrences of the given regular
--   expression. For example @a+@ matches the character @a@ one or
--   more times.
-- 
rep1 :: RegExp c -> RegExp c
rep1 r = r `seq` rep r

-- |
-- Matches the given regular expression or the empty word. Optional
-- expressions are usually written @a?@ but could also be written
-- @(|a)@, that is, as alternative between 'eps' and @a@.
-- 
opt :: RegExp c -> RegExp c
opt r = eps `alt` r

-- |
-- Matches a regular expression a given number of times. For example,
-- the regular expression @a{4,7}@ matches the character @a@ four to
-- seven times. If the minimal and maximal occurences are identical,
-- one can be left out, that is, @a{2}@ matches two occurrences of the
-- character @a@.
-- 
-- Numerical bounds are implemented via translation into ordinary
-- regular expressions. For example, @a{4,7}@ is translated into
-- @aaaaa?a?a?@.
-- 
brep :: (Int,Int) -> RegExp c -> RegExp c
brep (n,m) r =
  foldr seq (foldr seq eps (replicate (m-n) (opt r))) (replicate n r)
