{
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Parser ( parse ) where

import RegExp hiding ( scan )
import Data.String

instance IsString (RegExp Char)
 where fromString = parse

parse :: String -> RegExp Char
parse = parseTokens . scan

data Token = Sym Char | Ast | Dot | Bar | L | R
           | Pls | Que | Bounds (Int,Int)

scan :: String -> [Token]
scan = insertDots . process
 where
  process []            = []

  process ('\\':'d':cs) = error "TODO: implement backslash"

  process ('{':cs)      = case reads cs of
                            (n,',':s1) : _ ->
                                case reads s1 of
                                  (m,'}':s2) : _ -> Bounds (n,m) : process s2
                                  _ -> parseError []
                            _ -> parseError []

  process (c:cs)        = token c : process cs

token :: Char -> Token
token '*'  = Ast
token '|'  = Bar
token '('  = L
token ')'  = R
token '?'  = Que
token '+'  = Pls
token c    = Sym c

insertDots :: [Token] -> [Token]
insertDots []                     = []
insertDots [t]                    = [t]
insertDots (a:ts@(b:_))
  | dotFollows a && dotPrecedes b = a : Dot : insertDots ts
  | otherwise                     = a : insertDots ts

dotPrecedes :: Token -> Bool
dotPrecedes (Sym _) = True
dotPrecedes L       = True
dotPrecedes _       = False

dotFollows :: Token -> Bool
dotFollows Bar = False
dotFollows L   = False
dotFollows _   = True

parseError :: [Token] -> a
parseError _ = error "cannot parse regular expression"
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
       sym    { Sym $$ }
       '*'    { Ast }
       '.'    { Dot }
       '|'    { Bar }
       '('    { L }
       ')'    { R }
       '+'    { Pls }
       '?'    { Que }
       bounds { Bounds $$ }

%right '|'
%right '.'
%right '*' '+' '?' bounds

%%

RegExp : {- empty -}       { epsilon }
       | sym               { symbol $1 }
       | RegExp '*'        { star $1 }
       | RegExp '.' RegExp { $1 .*. $3 }
       | RegExp '|' RegExp { $1 .+. $3 }
       | '(' RegExp ')'    { $2 }
       | RegExp '+'        { plus $1 }
       | RegExp '?'        { optional $1 }
       | RegExp bounds     { bounded $1 $2 }
