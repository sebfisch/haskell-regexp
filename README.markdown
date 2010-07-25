% Weighted RegExp Matching

Efficient regular expression matching can be beautifully
simple. Revisiting ideas from theoretical computer science, it can be
implemented with linear worst-case time and space bounds in the purely
functional programming language [Haskell].

[Haskell]: http://hackage.haskell.org/platform/
[semirings]: http://en.wikipedia.org/wiki/Semiring

# Background

Since Plato wrote about philosophy in the form of [dialogues], authors
have used this literary form in didactic writing to convey their
ideas. The 15th [International Conference on Functional
Programming][ICFP] features an article on Regular Expressions written
as a play, [A Play on Regular Expressions][paper], which is meant to
be [elegant, instructive, and fun][Pearl]. The play discusses an
efficient, purely functional algorithm for matching regular
expressions. By generalizing from Booleans to arbitrary [semirings],
this algorithm implements various matching policies for weighted
regular expressions.

[dialogues]: http://en.wikipedia.org/wiki/Socratic_dialogue
[ICFP]: http://www.icfpconference.org/icfp2010/
[Pearl]: http://web.cecs.pdx.edu/~apt/icfp09_cfp.html#pearls
[paper]: regexp-play.pdf

# Installation

An implementation of the ideas discussed in the Play on Regular
Expressions is available as a Haskell library. It is implemented in
pure Haskell rather than as a binding to an external library so you do
not need to install an external regular expression library to use it.

<table class="installation">

<tr><td>

[![Download
Haskell](http://hackage.haskell.org/platform/icons/button-100.png)](http://hackage.haskell.org/platform)

</td><td>

However, you need Haskell in order to use this library. By installing
the [Haskell Platform][Haskell] you get a Haskell compiler with an
interactive environment as well as the package manager `cabal-install`
and various pre-installed packages.

</td></tr><tr><td>

<img src="http://hackage.haskell.org/images/Cabal-light.png"
     alt="Cabal" width="195" height="71" />

</td><td>

You can install the weighted regexp library by typing the following
into a terminal:

    bash# cabal update
    bash# cabal install weighted-regexp

</td></tr>

</table>

# Correctness

The matching algorithm computes the same result as a simple inductive
specification (given in the [Play on Regular Expressions][paper]) but
is [more efficient](#performance) than a direct translation of this
specification into Haskell. Although the ideas behind the algorithm
are not new but based on proven results from theoretical computer
science, there is no correctness proof for the equivalence of the
Haskell implementation of the algorithm with its specification. It is
therefore confirmed using tests.

Is difficult (and tedious) to write tests manually that cover all
interesting apsects of regular expression matching. Fortunately,
[QuickCheck] helps to generate such tests automatically and [Haskell
Program Coverage (HPC)][HPC] can be used to monitor test coverage.

[QuickCheck]: http://www.cse.chalmers.se/~rjmh/QuickCheck/
[HPC]: http://www.haskell.org/ghc/docs/latest/html/users_guide/hpc.html

You can install the `weighted-regexp` library along with a test
program as follows:

    bash# cabal install weighted-regexp -fQuickCheck

Using the `QuickCheck` flag results in an additional program that you
can use to test the implementation. It will test 

  * the algebraic laws of semirings for all defined semirings,

  * the equivalence of the matching algorithm with the specification
    both for full and partial matchings.

Moreover, it will test

  * the parser that provides common syntactic sugar like bounded
    repetitions and character classes, and

  * the use of the library to recognize non-regular languages using
    infinite regular expressions.

In order to generate an HPC report you need to download the sources of
the `weighted-regexp` package. But you may as well consult the
[pregenerated coverage report][coverage] instead of generating it yourself.

[coverage]: quickcheck/hpc_index.html

# Performance

# Development


