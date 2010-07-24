Regular Expressions in Haskell
==============================

This library provides a simple and fast regular expression matcher
that is implemented in Haskell without binding to external libraries.

There are different ways to implement regular expression
matching. Backtracking algorithms are simple but need bookkeeping
overhead for nondeterministic search. One can use [deterministic
finite automata (DFA)](http://swtch.com/~rsc/regexp/regexp1.html) to
match regular expressions faster. But for certain regular expressions
these DFA are exponentially large which sometimes leads to prohibitive
memory requirements.

We use a smart and simple algorithm to generate a DFA from a regular
expression and do not generate the DFA completely but on the fly while
parsing. This leads to a linear-time deterministic algorithm with
constant space requirements. More specifically, the run time is
limited by the product of the sizes of the regular expression and the
string and the memory is limited by the size of the regular
expression.

