{
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Text.RegExp.Parser () where

import Text.RegExp.Data

import Data.String
import Data.Char ( isSpace, toLower, isAlphaNum, isDigit )

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
       cls { Cls $$ }
       '.' { Dot }

%right '|'
%right seq
%right '*' '+' '?' bnd

%%

RegExp : {- empty -}       { epsilon }
       | sym               { char $1 }
       | RegExp '*'        { star $1 }
       | RegExp seq RegExp { $1 .*. $3 }
       | RegExp '|' RegExp { $1 .+. $3 }
       | '(' RegExp ')'    { $2 }
       | RegExp '+'        { plus $1 }
       | RegExp '?'        { optional $1 }
       | RegExp bnd        { bounded $1 $2 }
       | cls               { uncurry symbol $1 }
       | '.'               { symbol "." (const True) }

{
instance IsString (RegExp Char)
 where fromString = parse

parse :: String -> RegExp Char
parse = parseTokens . scan

data Token = Seq | Sym Char | Ast | Bar | L | R
           | Pls | Que | Bnd (Int,Int)
           | Cls (String, Char -> Bool) | Dot


token :: Char -> Token
token '*'  = Ast
token '|'  = Bar
token '('  = L
token ')'  = R
token '?'  = Que
token '+'  = Pls
token '.'  = Dot
token c    = Sym c

scan :: String -> [Token]
scan = insertSeqs . process

insertSeqs :: [Token] -> [Token]
insertSeqs []           = []
insertSeqs [t]          = [t]
insertSeqs (a:ts@(b:_))
  | lseq a && rseq b    = a : Seq : insertSeqs ts
  | otherwise           = a : insertSeqs ts

lseq :: Token -> Bool
lseq Bar = False
lseq L   = False
lseq _   = True

rseq :: Token -> Bool
rseq (Sym _) = True
rseq L       = True
rseq (Cls _) = True
rseq Dot     = True
rseq _       = False

process :: String -> [Token]
process []            = []

process ('\\':c:cs)
  | isSymClassChar c  = Cls ('\\':[c], symClassPred c) : process cs

process ('\\':c:cs)   = Sym c : process cs

process ('{':cs)      = case reads cs of
                          (n,'}':s1) : _ -> Bnd (n,n) : process s1
                          (n,',':s1) : _ ->
                              case reads s1 of
                                (m,'}':s2) : _ -> Bnd (n,m) : process s2
                                _              -> token '{' : process cs
                          _              -> token '{' : process cs

process ('[':'^':cs)  = Cls ("[^"++s, not.p) : process xs
 where (s,p,xs) = processCls cs

process ('['    :cs)  = Cls ('[':s, p) : process xs
 where (s,p,xs) = processCls cs

process (c:cs)        = token c : process cs

processCls :: String -> (String, Char -> Bool, String)

processCls []           = parseError []

processCls (']':cs)     = ("]", const False, cs)

processCls ('\\':c:cs)
  | isSymClassChar c    = ('\\':c:s, \x -> symClassPred c x || p x, xs)
 where (s,p,xs) = processCls cs

processCls ('\\':c:cs)  = ('\\':c:s, \x -> x==c || p x, xs)
 where (s,p,xs) = processCls cs

processCls (c:'-':e:cs) | e /= ']'
                        = (c:'-':e:s, \d -> (c<=d && d<=e) || p d, xs)
 where (s,p,xs) = processCls cs

processCls (c:cs)       = (c:s, \b -> b==c || p b, xs)
 where (s,p,xs) = processCls cs

isSymClassChar :: Char -> Bool
isSymClassChar = (`elem`"wWdDsS")

symClassPred :: Char -> Char -> Bool
symClassPred 'w' = isWordChar
symClassPred 'd' = isDigit
symClassPred 's' = isSpace
symClassPred  c  = not . symClassPred (toLower c)

isWordChar :: Char -> Bool
isWordChar c = c == '_' || isAlphaNum c

parseError :: [Token] -> a
parseError _ = error "cannot parse regular expression"
}
