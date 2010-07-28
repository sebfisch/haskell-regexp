% Weighted RegExp Matching

Efficient regular expression matching can be beautifully
simple. Revisiting ideas from theoretical computer science, it can be
implemented with linear worst-case time and space bounds in the purely
functional programming language [Haskell].

[Haskell]: http://hackage.haskell.org/platform/
[semirings]: http://en.wikipedia.org/wiki/Semiring

# Background

Since Plato wrote about philosophy in the form of [dialogues], authors
have used this literary form to convey their ideas. The 15th
[International Conference on Functional Programming][ICFP] features an
article on Regular Expressions written as a play, [A Play on Regular
Expressions][paper], which is meant to be [elegant, instructive, and
fun][Pearl]. The play discusses an efficient, purely functional
algorithm for matching regular expressions. By generalizing from
Booleans to arbitrary [semirings], this algorithm implements various
matching policies for weighted regular expressions.

[dialogues]: http://en.wikipedia.org/wiki/Socratic_dialogue
[ICFP]: http://www.icfpconference.org/icfp2010/
[Pearl]: http://web.cecs.pdx.edu/~apt/icfp09_cfp.html#pearls
[paper]: regexp-play.pdf

# Installation

An implementation of the ideas discussed in the Play on Regular
Expressions is available as a Haskell library. It is implemented in
pure Haskell rather than as a binding to an external library so you do
not need to install an external regular expression library to use it.

<table><tr><td>

<a href="http://hackage.haskell.org/platform">
<img src="http://hackage.haskell.org/platform/icons/button-100.png" />
</a>

</td><td>

However, you need Haskell in order to use this library. By installing
the [Haskell Platform][Haskell] you get a Haskell compiler with an
interactive environment as well as the package manager `cabal-install`
and various pre-installed packages.

</td></tr><tr><td>

<img src="http://hackage.haskell.org/images/Cabal-light.png"
     alt="Cabal" width="195" height="71" />

</td><td>

You can install the `weighted-regexp` library by typing the following
into a terminal:

    bash# cabal update
    bash# cabal install weighted-regexp

</td></tr></table>

This will install the current version. The differences between
versions are listed in the [changelog].

[changelog]: http://sebfisch.github.com/haskell-regexp/CHANGES.html

# Correctness

The matching algorithm computes the same result as a simple inductive
specification (given in the [Play on Regular Expressions][paper]) but
is [more efficient](#performance) than a direct translation of this
specification into Haskell. Although the ideas behind the algorithm
are not new but based on proven results from theoretical computer
science, there is no correctness proof for the equivalence of the
Haskell implementation of the algorithm with its specification. The
equivalence is therefore confirmed by testing.

It is difficult (and tedious) to write tests manually that cover all
interesting apsects of regular expression matching. Therefore,
[QuickCheck] is used to generate tests automatically and [Haskell
Program Coverage (HPC)][HPC] is used to monitor test coverage.

[QuickCheck]: http://www.cse.chalmers.se/~rjmh/QuickCheck/
[HPC]: http://www.haskell.org/ghc/docs/latest/html/users_guide/hpc.html

You can install the `weighted-regexp` library along with a test
program as follows:

    bash# cabal install weighted-regexp -fQuickCheck

Using the `QuickCheck` flag results in an additional program that you
can use to test the implementation. The program tests 

  * the algebraic laws of semirings for all defined semirings, and

  * the equivalence of the matching algorithm with the specification
    both for full and partial matchings.

For testing the equivalence, QuickCheck generates random regular
expressions and compares the result of the matching algorithm with the
result of its specification on random words. Moreover, the program
tests

  * the parser that provides common syntactic sugar like bounded
    repetitions and character classes,

  * the use of the library to recognize non-regular languages using
    infinite regular expressions, and

  * a combinator for parsing permutation sequences, that is, sequences
    of regular expressions in arbitrary order.

For a more detailed description of the tested properties consider the
[source code][quickcheck.lhs] of the test program. In order to
generate an HPC report you need to download the sources of the
`weighted-regexp` package. But you may as well consult the
[pregenerated coverage report][coverage] instead of generating one
yourself.

[quickcheck.lhs]: http://github.com/sebfisch/haskell-regexp/blob/master/src/quickcheck.lhs
[coverage]: http://sebfisch.github.com/haskell-regexp/quickcheck/hpc_index.html

# Performance

The matching algorithm provided by this library is usually slower than
other libraries like [pcre] but has a better asymptotic
complexity. There are no corner cases for which matching takes forever
or eats all available memory. More specifically, the worst-case run
time for matching a word against a regular expression is linearly
bounded by the length of the word and the size of the regular
expression. It is in *O(nm)* if *n* is the length of the word and *m*
the size of the expression. The memory requirements are independent of
the length of the word and linear in the size of the regular
expression, that is, in *O(m)*. Therefore, this library provides
similar asymptotic complexity guarantees as Google's [re2].

[pcre]: http://www.pcre.org/
[re2]: http://code.google.com/p/re2/

Here are timings that have been obtained (on a MacBook) with the
current version of the library.

       input               regexp            run time     memory
------------------- --------------------- -------------- --------
 100 MB of a's       `.*`                  6s (16 MB/s)    1 MB
 5000 a's            `(a?){5000}a{5000}`   12s             5 MB
 ~2M a's and b's     `.*a.{20}a.*`         3.5s            2 MB

The first example measures the search speed for a simple regular
expression with a long string. There is room for improvement. No time
has been invested yet to improve the performance of the library with
regard to constant factors.

The second example demonstrates the good asymptotic complexity of the
algorithm. Unlike a backtracking implementation like [pcre] the
library finishes in reasonable time. However, the memory requirements
are higher than usual and on closer inspection one can see that 9 of
12 seconds are spent during garbage collection. This example uses a
large regular expression which leads to a lot of garbage in the
matching algorithm.

The third example pushes automata based approaches to the limit
because the deterministic finite automaton that corresponds to the
regular expression is exponentially large. The input has been chosen
to not match the expression but is otherwise random and probably
explores many different states of the automaton. The matching
algorithm produces states on the fly and discards them, hence, it is
fast in this example, in fact, faster than re2[^cpp]. 

[^cpp]: The following C++ program uses the [re2] library and needs
*4.5s* to match `.*a.{20}a.*` against a string of ~2M random a's ad
b's:

    <script src="http://gist.github.com/488543.js?file=re2.cpp"></script>

    Unlike the Haskell program, this program keeps the whole input,
    that is, the result of `getline`, in memory. Can [re2] match input
    on the fly?

The benchmarks above all use large input and two of them are
specifically designed as corner cases of typical matching
algorithms. The run time of matching more common regular expressions
against short input has been measured using [Criterion] in order to
get statistically robust results.

[Criterion]: http://www.serpentine.com/blog/2009/09/29/criterion-a-new-benchmarking-library-for-haskell/

You can install the `weighted-regexp` package with the `Criterion` flag to generate a program that executes the benchmarks described below:

    bash# cabal install weighted-regexp -fCriterion

You can call `criterion-re --help` to see how to use the generated
program. It tests three different examples:

  * a unique full match with a regular expression for phone numbers,

  * an ambiguous full match with a regular expression for sequences of
    HTML elements, and

  * a partial match with a regular expression for protein sequences in
    RNA.

For a more detailed explanation consider the [source
code][criterion.lhs] of the benchmark program.

[criterion.lhs]: http://github.com/sebfisch/haskell-regexp/blob/master/src/criterion.lhs

       matching  acceptance  #matchings  leftmost     longest  leftmost longest
--------------- ----------- ----------- ---------- ---------- -----------------
 unique full       [3.9 us]   [28.1 us]
 ambiguous full   [11.5 us]   [78.1 us]
 partial           [0.2 us]              [76.8 us]  [76.4 us]         [77.6 us]

Click on the numbers for a more detailed distribution of run times.

[3.9 us]:  http://sebfisch.github.com/haskell-regexp/criterion/full-accept-phone-densities-800x600.png
[28.1 us]: http://sebfisch.github.com/haskell-regexp/criterion/full-count-phone-densities-800x600.png
[11.5 us]: http://sebfisch.github.com/haskell-regexp/criterion/full-accept-html-densities-800x600.png
[78.1 us]: http://sebfisch.github.com/haskell-regexp/criterion/full-count-html-densities-800x600.png
[0.2 us]: http://sebfisch.github.com/haskell-regexp/criterion/partial-accept-rna-densities-800x600.png
[76.8 us]: http://sebfisch.github.com/haskell-regexp/criterion/partial-leftmost-rna-densities-800x600.png
[76.4 us]: http://sebfisch.github.com/haskell-regexp/criterion/partial-longest-rna-densities-800x600.png
[77.6 us]: http://sebfisch.github.com/haskell-regexp/criterion/partial-leftlong-rna-densities-800x600.png

# Collaboration

<table><tr><td>

<a href="http://github.com">
<img src="https://github.com/images/modules/header/logo.png" />
</a>

</td><td>

The source code of this library is on [github]. You can collaborate by
using it in your projects, report bugs and ask for new features in the
[issue tracker], or provide patches that implement pending issues.

</td></tr></table>

[github]: http://github.com/sebfisch/haskell-regexp
[issue tracker]: http://github.com/sebfisch/haskell-regexp/issues

The algorithm discussed in the [Play on Regular Expressions][paper]
has been implemented in different languages. In a series of two
[blog][blog] [posts][posts], Carl Friedrich Bolz describes a Python
implementation that uses a Just In Time (JIT) compiler to achieve
impressive performance. He compares his version with corresponding C++
and Java programs.

[blog]: http://morepypy.blogspot.com/2010/05/efficient-and-elegant-regular.html
[posts]: http://morepypy.blogspot.com/2010/06/jit-for-regular-expression-matching.html

For questions and feedback email [Sebastian
Fischer](mailto:mail@sebfisch.de).