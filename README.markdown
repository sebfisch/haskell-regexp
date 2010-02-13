Regular Expressions in Haskell
==============================

There are different ways to implement regular expression
matching. Backtracking algorithms are simple but need bookkeeping
overhead for nondeterministic search. One can use [deterministic
finite automata (DFA)](http://swtch.com/~rsc/regexp/regexp1.html) to
match regular expressions faster. But for certain regular expressions
these DFA are exponentially large which leads to sometime prohibitive
memory requirements.

We use a smart and simple algorithm to generate a DFA from a regular
expression but do not generate it completely but on the while
parsing. This leads to a linear-time deterministic algorithm with
constant space requirements. More specifically, the run time is
limited by the product of the sizes of the regular expression and the
string.

