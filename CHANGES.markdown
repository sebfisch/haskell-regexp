% Changelog for [`weighted-regexp`]

[`weighted-regexp`]: http://hackage.haskell.org/package/weighted-regexp

# 0.3.0.0

## Implemented workaround for [GHC ticket 4227]

Currently, GHC can SPECIALIZE functions only where they are defined.
The types `Leftmost`, `Longest`, and `LeftLong` are now defined in
separate modules to bring them into the scope of the matching
functions. Specialization makes the matching functions almost three
times faster for the types mentioned above.

This workaround allows to specialize the matching functions for types
defined in this package. Users, however, must use the matching
functions unspecialized for their own types.

Along with this change, the constructors of the matching types are no
longer exported.

# 0.2.0.0

## More general types for matching functions

The functions `fullMatch` and `partialMatch` now both have the type

    Weight a b w => RegExp a -> [b] -> w

whereas previously the signatures have been:

    fullMatch    :: Semiring w         => RegExp c -> [c] -> w
    partialMatch :: Weight c (Int,c) w => RegExp c -> [c] -> w

The change allows users to provide custom symbol weights in full
matchings and to do partial matchings with arbitrary symbols weights
instead of having to use only characters and their positions.

This generalization leads to a slight performance penalty in small
examples but has a negligible effect when matching large inputs.

## Renamed `accept` to `acceptFull`, added `acceptPartial`

Based on the more general `partialMatch` function, the function
`acceptPartial` was added for the `Bool` semiring. The `accept`
function has been appropriately renamed.

## Strict numeric semiring

The lazy definition of arithmetic operations for the `Numeric`
semiring has been dropped in favour of the more efficient standard
implementation. As a consequence, `matchingCount` no longer works with
infinite regular expressions.

## SPECIALIZE pragmas prevent memory leak

The generalization of the matching functions leads to a memory leak
that can be avoided by specializing them for concrete
semirings. Corresponding pragmas have been added for `Bool` and for
`Numeric` types but not for the more complex semirings defined in the
extra matching modules. It is unclear what is the best way to
specialize them too because the pragma must be placed in the module
where the matching functions are defined but, there, not all semirings
are in scope. See [GHC ticket 4227].

[GHC ticket 4227]: http://hackage.haskell.org/trac/ghc/ticket/4227

## Fixed mistake in Criterion benchmarks

In the group of partial matchings, the benchmark for `Bool`
accidentally used full matching. It now uses partial matching which,
unsurprisingly, is slower.

# 0.1.1.0

## added `noMatch`

`Text.RegExp` now provides a combinator

    noMatch :: RegExp c

which is an identity of `alt`. With this combinator, regular
expressions form a semiring with

    zero  = noMatch
    one   = eps
    (.+.) = alt
    (.*.) = seq_

A corresponding `Semiring` instance is not defined due to the lack of
an appropriate `Eq` instance.

## added `perm`

`Text.RegExp` now provides a combinator

    perm :: [RegExp c] -> RegExp c

that matches the given regular expressions in sequence. Each
expression must be matched exactly once but in arbitrary order. For
example, the regular expression

    perm (map char "abc")

is equivalent to `abc|acb|bca|bac|cba|cab` and represented as
`a(bc|cb)|b(ca|ac)|c(ba|ab)`.
