{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module      : Text.RegExp
-- Copyright   : Thomas Wilke, Frank Huch, and Sebastian Fischer
-- License     : BSD3
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- This library provides a simple and fast regular expression matcher
-- that is implemented in Haskell without binding to external
-- libraries.
-- 
-- There are different ways to implement regular expression
-- matching. Backtracking algorithms are simple but need bookkeeping
-- overhead for nondeterministic search. One can use deterministic
-- finite automata (DFA, see
-- <http://swtch.com/~rsc/regexp/regexp1.html>) to match regular
-- expressions faster. But for certain regular expressions these DFA
-- are exponentially large which sometimes leads to prohibitive memory
-- requirements.
-- 
-- We use a smart and simple algorithm to generate a DFA from a
-- regular expression and do not generate the DFA completely but on
-- the fly while parsing. This leads to a linear-time deterministic
-- algorithm with constant space requirements. More specifically, the
-- run time is limited by the product of the sizes of the regular
-- expression and the string and the memory is limited by the size of
-- the regular expression.
-- 
module Text.RegExp (

  module Data.Semiring, Weight(..),

  -- * Constructing regular expressions

  RegExp, fromString,

  eps, char, sym, psym, anySym, noMatch, alt, seq_, rep, rep1, opt, brep,

  perm,

  -- * Matching

  (=~), accept, matchingCount, fullMatch, partialMatch

  ) where

import Data.Semiring
import qualified Data.String

import Text.RegExp.Data
import Text.RegExp.Parser
import Text.RegExp.Matching

-- |
-- Parses a regular expression from its string representation. If the
-- 'OverloadedStrings' language extension is enabled, string literals
-- can be used as regular expressions without using 'fromString'
-- explicitly. Implicit conversion is especially useful in combination
-- with functions like '=~' that take a value of type @RegExp Char@ as
-- argument.
-- 
-- Here are some examples of supported regular expressions along with
-- an explanation what they mean:
-- 
--  * @a@ matches the character @a@
-- 
--  * @[abc]@ matches any of the characters @a@, @b@, or @c@. It is
--    equivalent to @(a|b|c)@, but @|@ can be used to specify
--    alternatives between arbitrary regular expressions, not only
--    characters.
-- 
--  * @[^abc]@ matches anything but the characters @a@, @b@, or @c@.
-- 
--  * @\\d@ matches a digit and is equivalent to @[0-9]@. Moreover,
--    @\\D@ matches any non-digit character, @\\s@ and @\\S@ match
--    space and non-space characters and @\\w@ and @\\W@ match word
--    characters and non-word characters, that is, @\\w@ abbreviates
--    @[a-zA-Z_]@.
-- 
--  * @a?@ matches the empty word or the character @a@, @a*@ matches
--    zero or more occurrences of @a@, and @a+@ matches one or more
--    @a@'s.
-- 
--  * @.@ (the dot) matches one arbitrary character.
-- 
--  * @a{4,7}@ matches four to seven occurrences of @a@, @a{2}@
--    matches two.
-- 
fromString :: String -> RegExp Char
fromString = Data.String.fromString

instance Data.String.IsString (RegExp Char) where
  fromString = parse

-- |
-- Matches a sequence of the given regular expressions in any
-- order. For example, the regular expression
-- 
-- @
-- perm (map char \"abc\")
-- @
-- 
-- has the same meaning as
-- 
-- @
-- abc|acb|bcc|bac|cba|cab
-- @
-- 
-- and is represented as
-- 
-- @
-- a(bc|cb)|b(ca|ac)|c(ba|ab)
-- @
-- 
perm :: [RegExp c] -> RegExp c
perm []  = eps
perm [r] = r
perm rs  = go rs []
 where
  go [p]    qs = p `seq_` perm qs
  go (p:ps) qs = (p `seq_` perm (ps ++ qs)) `alt` go ps (p:qs)

-- | 
-- Alias for 'accept' specialized for Strings. Useful in combination
-- with the 'IsString' instance for 'RegExp' 'Char'
-- 
(=~) :: RegExp Char -> String -> Bool
(=~) = accept

