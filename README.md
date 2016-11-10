# PEGC

PEGC is a parser generator that takes as input a grammar in a format similar to
that of [BNFC][], except with support for asymmetric choice, the `&` combinator,
the `!` combinator, and local failure via `raise`.

It outputs a C++ file that uses the [PEGTL][] library to parse the language
specified in the grammar you provided.

Contact me via my [email](taktoa@gmail.com), or by talking to me on FreeNode IRC
(my nick is `taktoa`, of course).

[BNFC]:  http://bnfc.digitalgrammars.com
[PEGTL]: https://github.com/ColinH/PEGTL
