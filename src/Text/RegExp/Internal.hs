-- |
-- Module      : Text.RegExp
-- Copyright   : Thomas Wilke, Frank Huch, and Sebastian Fischer
-- License     : BSD3
-- Maintainer  : Sebastian Fischer <mailto:sebf@informatik.uni-kiel.de>
-- Stability   : experimental
-- 
-- This module exports internal data types and matching functions. You
-- do not need to import it unless you want to write your own matching
-- algorithms.
-- 
module Text.RegExp.Internal (

  module Text.RegExp.Data, module Text.RegExp.Matching

  ) where

import Text.RegExp.Data
import Text.RegExp.Matching
