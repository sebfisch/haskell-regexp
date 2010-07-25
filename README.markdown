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
Expressions is available as a Haskell library.

<table class="installation">

<tr><td>

[![Download
Haskell](http://hackage.haskell.org/platform/icons/button-100.png)](http://hackage.haskell.org/platform)

</td><td>

You need to install Haskell in order to use this library. By
installing the [Haskell Platform][Haskell] you get a Haskell compiler
with an interactive environment as well as the package manager
`cabal-install` and various pre-installed packages.

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

# Performance

# Development


