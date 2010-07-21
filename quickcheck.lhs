{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}

We specify a `Monoid` instance for a `newtype` of lists.

> import Data.Monoid ( Monoid(..) )

We use QuickCheck version 1 for testing because version 2 cannot be
used in batch mode.

> import Test.QuickCheck
> import Test.QuickCheck.Batch
> import Control.Monad ( ap )
> import Data.Char ( ord )

We import the semiring properties in order to check them for the
defined instances.

> import Data.Semiring.Properties

Finally, we need the `RegExp` datatype, the `symWeight` function from
the `Weight` class, and the different semirings used for matching.

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
>  where
>   options = defOpt { no_of_tests = 1000 }

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
