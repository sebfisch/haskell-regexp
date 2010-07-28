% Changelog for `weighted-regexp`

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
