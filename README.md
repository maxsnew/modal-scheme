# SBPV

This repo contains an experimental implementation of a dynamically
typed version of Levy's Call-by-push-value (CBPV), which I'm currently
calling "SBPV" for "Scheme-by-push-value". SBPV is based on the
"Scheme-like" model we designed in [a paper on gradual typing][GTT]
using a CBPV-like as a metalanguage.

The basic idea of the language is that instead of being uni-typed,
SBPV is "dityped" in that there are 2 kinds of expressions: inert
values and effectful computations.  Values are S-expressions as in a
typical Scheme: they include symbols, boolean values, strings, ..., as
well as immutable cons pairs and "thunked" computations.  Computations
are effectful programs that interact with the stack. The current stack
is always either an opaque continuation (written `#:bind`) waiting for
the computation to `ret` a value to it, or is a value pushed onto a
stack. The basic primitive for interacting with the stack is
`case-lambda` which co-pattern matches on the stack to see if there
are any arguments left.  This stack-manipulation provides a slightly
lower level interface for implementing variable-arity functions than
typical Schemes do.

The language is implemented as a "#lang" in Racket, using the
[turnstile][turnstile] library.

# Repository Structure

1. The actual library is in the `sbpv` directory, which can be installed
using `raco`. It also contains a bare-bones stdlib.
2. More example code
can be found in the `examples` directory.
3. The `cbpv` directory
contains the beginnings of a typed version of the language.
4. The `fiddle` directory contains the beginnings of an interpreter
written in Haskell.
5. The `dreams` directory contains some 

# Performance

Performance is pretty bad. Macro-expansion can take a while, but worse
is the runtime performance, though I haven't looked into exactly why
the Racket's optimizer doesn't do better on it.

# The Name

Currently the lang is "sbpv". A better name is "fiddle", but I haven't
changed everything to reflect that yet.

[turnstile]: https://docs.racket-lang.org/turnstile/index.html
[GTT]: https://arxiv.org/abs/1811.02440
