-- |
-- Module      : Text.RegExp.Matching.LeftLong
-- Copyright   : Thomas Wilke, Frank Huch, and Sebastian Fischer
-- License     : BSD3
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- This module implements leftmost longest matching based on weighted
-- regular expressions. It should be imported qualified as the
-- interface resembles that provided by other matching modules.
-- 
module Text.RegExp.Matching.LeftLong (

  matching, 

  Matching, matchingIndex, matchingLength,

  LeftLong, getLeftLong

  ) where

import Text.RegExp
import Text.RegExp.Matching.LeftLong.Type

-- |
-- Subwords of words that match a regular expression are represented
-- as values of type 'Matching'.
-- 
data Matching = Matching {
 
  -- | Start index of the matching subword in the queried word.
  matchingIndex :: !Int,
 
  -- | Length of the matching subword.
  matchingLength :: !Int
 
  }
 deriving Eq

instance Show Matching
 where
  showsPrec _ m = showString "<index:" . shows (matchingIndex m)
                . showString " length:" . shows (matchingLength m)
                . showString ">"

-- |
-- Returns the leftmost longest of all non-empty matchings for a
-- regular expression in a given word. If the empty word is the only
-- matching its position is zero.
-- 
matching :: RegExp c -> [c] -> Maybe Matching
matching r = getLeftLong . partialMatch r . zip [(0::Int)..]

getLeftLong :: LeftLong -> Maybe Matching
getLeftLong Zero            =  Nothing
getLeftLong One             =  Just $ Matching 0 0
getLeftLong (LeftLong x y)  =  Just $ Matching x (y-x+1)

