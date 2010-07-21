> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}

We specify a `Monoid` instance for a `newtype` of lists.

> import Data.Monoid ( Monoid(..) )

We use QuickCheck version 1 for testing because version 2 cannot be
used in batch mode.

> import Test.QuickCheck
> import Test.QuickCheck.Batch
> import Control.Monad ( ap )
> import Data.Char ( ord )

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
>  do runTests "    semiring laws for Bool" options $
>       checks (semiring'laws :: Checks Bool)
>     runTests "     semiring laws for Int" options $
>       checks (semiring'laws :: Checks (Numeric Int))
>     runTests "semiring laws for Leftmost" options $
>       checks (semiring'laws :: Checks Leftmost)
>     runTests " semiring laws for Longest" options $
>       checks (semiring'laws :: Checks Longest)
>     runTests "semiring laws for LeftLong" options $
>       checks semiring'laws'LeftLong
>     runTests "    matcher spec with Bool" options $
>       checks match'spec'Bool
>     runTests "     matcher spec with Int" options $
>       checks match'spec'Int
>     runTests "matcher spec with Leftmost" options $
>       checks (match'spec :: Checks Leftmost)
>     runTests " matcher spec with Longest" options $
>       checks (match'spec :: Checks Longest)
>     runTests "matcher spec with LeftLong" options $
>       checks (match'spec :: Checks LeftLong)
>     runTests "      parse printed regexp" options $
>       [run parse'printed]
>  where
>   options = defOpt { no_of_tests = 1000, length_of_tests = 0 }

The `Arbitrary` instance for numeric types wraps the underlying
instance. We also provide one for `Char` which is not predefined.

> instance Arbitrary a => Arbitrary (Numeric a) where
>   arbitrary   = Numeric `fmap` arbitrary
>
> instance Arbitrary Char where
>   arbitrary   = elements "abcde"
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
>   mtch = match
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
> regexp 0 = frequency [ (1,return eps)
>                      , (2,sym `fmap` arbitrary) ]
> regexp n = frequency [ (1,return eps)
>                      , (2,sym `fmap` arbitrary)
>                      , (3,alt  `fmap` subexp `ap` subexp)
>                      , (6,seq_ `fmap` subexp `ap` subexp)
>                      , (3,rep  `fmap` regexp (n-1)) ]
>  where subexp = regexp (n `div` 2)

The specification of the matching function is defined by exhaustive
search.

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

As a partial check for the parser, we check whether the representation
generated by the `Show` instance of regular expressions can be parsed
back and yields the original expression. This does not check syntactic
sugar like `a?` or `[a-z]` which should be checked separately.

> parse'printed :: RegExp Char -> Bool
> parse'printed r = fromString (show r) == r
