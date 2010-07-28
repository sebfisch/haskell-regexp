
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
>       | (name, re, str) <-
>         [ ("phone", phone're, phone'str)
>         , ("html" , html're , html'str)
>         ]
>       ]
>     | (mode, call) <-
>       [ ("accept", whnf . acceptFull)
>       , ("count" , whnf . (matchingCount :: RegExp Char -> String -> Int))
>       ]
>     ]
>   , bgroup "partial"
>     [ bgroup mode
>       [ bench name $ call re str
>       | (name, re, str) <-
>         [ ("rna", rna're, rna'str)
>         ]
>       ]
>     | (mode, call) <-
>       [ ("accept"  , whnf . acceptPartial)
>       , ("leftmost", whnf . Leftmost.matching)
>       , ("longest" , whnf . Longest.matching)
>       , ("leftlong", whnf . LeftLong.matching)
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
> html're = "(<\\w*>.*</\\w*>)*"

This expressions matches the string below in two different ways.

> html'str :: String
> html'str = "<p>some</p><p>text</p>"

To benchmark partial matchings we search for a protein sequence in an
RNA sequence. Protein sequences start with `AUG`, followed by codons
(triplets) built from the bases adenin (`A`), cytosine (`C`), guanin
(`G`), and uracil (`U`), and end with `UAG`, `UGA`, or `UAA`.

> rna're :: RegExp Char
> rna're = "AUG([ACGU][ACGU][ACGU])*(UAG|UGA|UAA)"

For example, the following RNA sequence contains the protein sequence
`AUGACACUUGAAUGA`.

> rna'str :: String
> rna'str = "UUACGGAUGACACUUGAAUGACUGA"

