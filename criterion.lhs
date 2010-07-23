
> {-# LANGUAGE OverloadedStrings #-}

We use Criterion to run a number of micro benchmarks that match
different regular expressions against strings.

> import Text.RegExp
> import Text.RegExp.Matching.Leftmost as Leftmost
> import Text.RegExp.Matching.Longest  as Longest
> import Text.RegExp.Matching.LeftLong as LeftLong
>
> import Criterion.Main
>
> main :: IO ()
> main = defaultMain
>   [ bgroup "full"
>     [ bgroup mode
>       [ bench name $ call re str
>       | (name,re,str) <-
>         [ ("phone", phone're, phone'str)
>         , ("html", html're, html'str)
>         ]
>       ]
>     | (mode,call) <-
>       [ ("accept", whnf . accept)
>       , ("count" , whnf . (matchingCount :: RegExp Char -> String -> Int))
>       ]
>     ]
>   ]

The following regular expression for phone numbers matches uniquely
against phone numbers like the one given below.

> phone're :: RegExp Char
> phone're = "[0-9]+(-[0-9]+)*"
>
> phone'str :: String
> phone'str = "0431-880-7267"

As an example for an ambiguous match we match the following regular
expression wich reminds one of HTML documents.

> html're :: RegExp Char
> html're = "(<[^<>]*>.*</[^<>]*>)*"

This expressions matches the string below in two different ways.

> html'str :: String
> html'str = "<p>some</p><p>text</p>"

