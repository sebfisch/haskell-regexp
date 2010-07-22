> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}

We specify a `Monoid` instance for a `newtype` of lists.

> import Data.Monoid ( Monoid(..) )

We use QuickCheck version 1 for testing because version 2 cannot be
used in batch mode.

> import Test.QuickCheck
> import Test.QuickCheck.Batch
> import Control.Monad ( ap )
> import Data.Char ( chr, ord )

We import the semiring properties in order to check them for the
defined instances. We also define our own `sum` function for
semirings.

> import Data.Semiring.Properties
> import Prelude hiding ( sum )

Finally, we need the `RegExp` datatype, the `symWeight` function from
the `Weight` class, and the different semirings used for matching.

> import Text.RegExp
> import Text.RegExp.Data
> import Text.RegExp.Matching.Leftmost
> import Text.RegExp.Matching.Longest
> import Text.RegExp.Matching.LeftLong

The `main` function runs all tests defined in this program.

> main :: IO ()
> main = 
>  do runChecks "semiring laws for Bool" (semiring'laws :: Checks Bool)
>     runChecks "semiring laws for Int" (semiring'laws :: Checks (Numeric Int))
>     runChecks "semiring laws for Leftmost" (semiring'laws :: Checks Leftmost)
>     runChecks "semiring laws for Longest" (semiring'laws :: Checks Longest)
>     runChecks "semiring laws for LeftLong" semiring'laws'LeftLong
>     runChecks "full match with Bool" match'spec'Bool
>     runChecks "full match with Int" match'spec'Int
>     runChecks "full match with Leftmost" (match'spec :: Checks Leftmost)
>     runChecks "full match with Longest" (match'spec :: Checks Longest)
>     runChecks "full match with LeftLong" (match'spec :: Checks LeftLong)
>     runChecks "parse printed regexp" (Checks [run parse'printed])
>     runChecks "lazy infinite regexps" infinite'regexp'checks
>  where
>   options = defOpt { no_of_tests = 1000, length_of_tests = 60 }
>   runChecks s = runTests (pad s) options . checks
>   pad s = replicate (30-length s) ' ' ++ s

The `Arbitrary` instance for numeric types wraps the underlying
instance. We also provide one for `Char` which is not predefined.

> instance Arbitrary a => Arbitrary (Numeric a) where
>   arbitrary   = Numeric `fmap` arbitrary
>
> instance Arbitrary Char where
>   arbitrary   = elements "abcde \\|*+?.[]{}"
>   coarbitrary = coarbitrary . ord

We provide generic `Semiring` instances for the semirings used for
matching.

> instance Arbitrary Leftmost where
>   arbitrary   = weight
>
> instance Arbitrary Longest where
>   arbitrary   = weight
>
> instance Arbitrary LeftLong where
>   arbitrary   = weight
>
> weight :: forall s . (Weight Char (Int,Char) s, Arbitrary s) => Gen s
> weight = oneof [ return zero
>                , return one
>                , (.+.) `fmap` arbitrary `ap` arbitrary
>                , (.*.) `fmap` arbitrary `ap` arbitrary
>                , symWeight `fmap` (arbitrary :: Gen (Char -> s))
>                              `ap` (arbitrary :: Gen (Int,Char)) ]

We define a list of `Checks` for the semiring laws.

> semiring'laws :: (Arbitrary s, Show s, Semiring s) => Checks s
> semiring'laws = mconcat [ prop2 plus'comm
>                         , prop1 left'zero
>                         , prop3 add'assoc
>                         , prop1 left'one
>                         , prop1 right'one
>                         , prop3 mul'assoc
>                         , prop3 left'distr
>                         , prop3 right'distr
>                         , prop1 left'ann
>                         , prop1 right'ann
>                         ]

`Checks` is a `newtype` for a list of batch tests with a phantom type
that can be used in definitions of the properties.

> newtype Checks a = Checks { checks :: [TestOptions -> IO TestResult] }
>  deriving ( Monoid )

We define the auxiliary functions to create semiring properties with
different arities.

> prop1 :: (Arbitrary s, Show s, Testable a) => (s -> a) -> Checks s
> prop1 prop = Checks [run prop]
>
> prop2 :: (Arbitrary s, Show s, Testable a) => (s -> s -> a) -> Checks s
> prop2 prop = Checks [run prop]
>
> prop3 :: (Arbitrary s, Show s, Testable a) => (s-> s -> s -> a) -> Checks s
> prop3 prop = Checks [run prop]

The `LeftLong` type satisfies the distributive laws only with a
precondition on all involved multiplications:

    mult'LeftLong'pre :: LeftLong -> LeftLong -> Bool
    mult'LeftLong'pre (LeftLong a b) (LeftLong c d)  =  a<b && b+1==c && c < d
    mult'LeftLong'pre _              _               =  True

This precondition is satisfied for all multiplications during regular
expression matching because multiplication combines adjacent matches.

We define a variant of `semiring'laws` with this precondition on the
distributive laws.

> semiring'laws'LeftLong :: Checks LeftLong
> semiring'laws'LeftLong = mconcat
>   [ prop2 plus'comm
>   , prop1 left'zero
>   , prop3 add'assoc
>   , prop1 left'one
>   , prop1 right'one
>   , prop3 mul'assoc
>   , prop3 (\a b c -> mult'LeftLong'pre a (b .+. c) &&
>                      mult'LeftLong'pre a b &&
>                      mult'LeftLong'pre a c
>                  ==> left'distr a b c)
>   , prop3 (\a b c -> mult'LeftLong'pre (a .+. b) c &&
>                      mult'LeftLong'pre a c &&
>                      mult'LeftLong'pre b c
>                  ==> right'distr a b c)
>   , prop1 left'ann
>   , prop1 right'ann
>   ]

Now we turn to the correctness of the `match` function. In order to
check it, we compare it with a executable specification which is
correct by definition:

> match'spec :: forall s . Semiring s => Checks s
> match'spec = Checks [run (check'match'spec mtch)]
>  where
>   mtch :: RegExp Char -> String -> s
>   mtch = fullMatch
>
> check'match'spec :: Semiring s
>                  => (RegExp Char -> String -> s)
>                  -> RegExp Char -> String -> Property
> check'match'spec mtch r s = length s < 7 ==> mtch r s == matchSpec r s

For `Bool` and `Int`, we use special functions in order to achieve the
corresponding code coverage.

> match'spec'Bool :: Checks Bool
> match'spec'Bool = Checks [run (check'match'spec (=~))]
>
> match'spec'Int :: Checks (Numeric Int)
> match'spec'Int = Checks [run (check'match'spec mtch)]
>  where
>   mtch :: RegExp Char -> String -> Numeric Int
>   mtch r s = Numeric (matchingCount r s)

To make this work, we need an `Arbitrary` instance for regular
expressions.

> instance Arbitrary (RegExp Char) where
>   arbitrary = sized regexp
>
> regexp :: Int -> Gen (RegExp Char)
> regexp 0 = frequency [ (1, return eps)
>                      , (4, char `fmap` simpleChar) ]
> regexp n = frequency [ (1, regexp 0)
>                      , (3, alt  `fmap` subexp `ap` subexp)
>                      , (6, seq_ `fmap` subexp `ap` subexp)
>                      , (3, rep  `fmap` regexp (n-1))
>                      , (7, fromString `fmap` parsedRegExp n) ]
>  where subexp = regexp (n `div` 2)
>
> simpleChar :: Gen Char
> simpleChar = elements "abcde"
>
> parsedRegExp :: Int -> Gen String
> parsedRegExp 0 = symClass
> parsedRegExp n = frequency [ (4, symClass)
>                            , (2, (++"?") `fmap` subexp)
>                            , (2, (++"+") `fmap` subexp)
>                            , (1, mkBrep1 =<< subexp)
>                            , (1, mkBrep2 =<< subexp) ]
>  where
>   subexp = (($"") . showParen True . shows)
>     `fmap` (resize (n-1) arbitrary :: Gen (RegExp Char))
>
>   mkBrep1 r = do x <- elements [0..3] :: Gen Int
>                  return $ r ++ "{" ++ show x ++ "}"
>
>   mkBrep2 r = do x <- elements [0..2] :: Gen Int
>                  y <- elements [0..2] :: Gen Int
>                  return $ r ++ "{" ++ show x ++ "," ++ show (x+y) ++ "}"
>
> symClass :: Gen String
> symClass = frequency [ (1, specialChar)
>                      , (2, do n <- choose (0,3)
>                               cs <- sequence (replicate n charClass)
>                               s <- (["","^"]!!) `fmap` choose (0,1)
>                               return $ "[" ++ s ++ concat cs ++ "]") ]
>  where
>   specialChar = elements (map (:[]) "." ++
>                           map (\c -> '\\':[c]) "abcdewWdDsS \\|*+?.[]{}")
>   charClass   = oneof [ (:[]) `fmap` simpleChar
>                       , specialChar
>                       , do x <- simpleChar
>                            y <- simpleChar
>                            return $ x:"-" ++ [chr (ord x+ord y-ord 'a')] ]

The specification of the matching function is defined inductively on
the structure of a regular expression. It uses exhaustive search to
find all possibilities to match a regexp against a word.

> matchSpec :: Semiring s => RegExp c -> [c] -> s
> matchSpec (RegExp r) = spec (reg r)
>  where
>   spec Eps        u  =  if null u then one else zero
>   spec (Sym _ f)  u  =  case u of [c] -> f c; _ -> zero
>   spec (Alt p q)  u  =  spec (reg p) u .+. spec (reg q) u
>   spec (Seq p q)  u  =
>     sum [ spec (reg p) u1 .*. spec (reg q) u2 | (u1,u2) <- split u ]
>   spec (Rep p)    u  =
>     sum [ prod [ spec (reg p) ui | ui <- ps] | ps <- parts u ]
>
> sum, prod :: Semiring s => [s] -> s
> sum   =  foldr (.+.) zero
> prod  =  foldr (.*.) one
>
> split :: [a] -> [([a],[a])]
> split []      =  [([],[])]
> split (c:cs)  =  ([],c:cs) : [ (c:s1,s2) | (s1,s2) <- split cs ]
>
> parts :: [a] ->  [[[a]]]
> parts []      =  [[]]
> parts [c]     =  [[[c]]]
> parts (c:cs)  =  concat [ [(c:p):ps,[c]:p:ps] | p:ps <- parts cs ]

As a check for the parser, we check whether the representation
generated by the `Show` instance of regular expressions can be parsed
back and yields the original expression.

> parse'printed :: RegExp Char -> Bool
> parse'printed r = fromString (show r) == r

We can also match infinite regular expressions lazily to recognize
context-free or even context-sensitive languages.

> infinite'regexp'checks :: Checks Bool
> infinite'regexp'checks = Checks [run context'free, run context'sensitive]

As an example for a context-free language, we recognize 
${a^nb^n | n >= 0}$

> context'free :: String -> Bool
> context'free s = isInAnBn s == (anbn =~ s)
>
> isInAnBn :: String -> Bool
> isInAnBn s = all (=='a') xs && all (=='b') ys && length xs == length ys
>  where (xs,ys) = break (=='b') s
>
> anbn :: RegExp Char
> anbn = eps `alt` seq_ "a" (anbn `seq_` "b")

As an example for a context-sensitive language we use
${a^nb^nc^n | n >= 0}$

> context'sensitive :: String -> Bool
> context'sensitive s = isInAnBnCn s == (anbncn =~ s)
>
> isInAnBnCn :: String -> Bool
> isInAnBnCn s = all (=='a') xs && all (=='b') ys && all (=='c') zs
>             && length xs == length ys && length ys == length zs
>  where (xs,l)  = break (=='b') s
>        (ys,zs) = break (=='c') l
>
> anbncn :: RegExp Char
> anbncn = mkAnBnCn 0
>  where
>   mkAnBnCn n = fromString (replicate n 'b' ++ replicate n 'c')
>          `alt` seq_ "a" (mkAnBnCn (n+1))
