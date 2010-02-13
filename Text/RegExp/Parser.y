{
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Text.RegExp.Parser ( parse ) where

import Text.RegExp.Data

import Data.String
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
       sym { Sym $$ }
       '*' { Ast }
       seq { Seq }
       '|' { Bar }
       '(' { L }
       ')' { R }
       '+' { Pls }
       '?' { Que }
       bnd { Bnd $$ }

%right '|'
%right seq
%right '*' '+' '?' bnd

%%

RegExp : {- empty -}       { epsilon }
       | sym               { symbol $1 }
       | RegExp '*'        { star $1 }
       | RegExp seq RegExp { $1 .*. $3 }
       | RegExp '|' RegExp { $1 .+. $3 }
       | '(' RegExp ')'    { $2 }
       | RegExp '+'        { plus $1 }
       | RegExp '?'        { optional $1 }
       | RegExp bnd        { bounded $1 $2 }

{
instance IsString (RegExp Char)
 where fromString = parse

parse :: String -> RegExp Char
parse = parseTokens . scan

data Token = Sym Char | Ast | Seq | Bar | L | R
           | Pls | Que | Bnd (Int,Int)

scan :: String -> [Token]
scan = insertSeqs . process
 where
  process []            = []

  process ('\\':cs) = error "TODO: implement backslash"

  process ('{':cs)      = case reads cs of
                            (n,'}':s1) : _ -> Bnd (n,n) : process s1
                            (n,',':s1) : _ ->
                                case reads s1 of
                                  (m,'}':s2) : _ -> Bnd (n,m) : process s2
                                  _ -> token '{' : process cs
                            _ -> token '{' : process cs

  process (c:cs)        = token c : process cs

token :: Char -> Token
token '*'  = Ast
token '|'  = Bar
token '('  = L
token ')'  = R
token '?'  = Que
token '+'  = Pls
token c    = Sym c

insertSeqs :: [Token] -> [Token]
insertSeqs []                     = []
insertSeqs [t]                    = [t]
insertSeqs (a:ts@(b:_))
  | dotFollows a && dotPrecedes b = a : Seq : insertSeqs ts
  | otherwise                     = a : insertSeqs ts

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
