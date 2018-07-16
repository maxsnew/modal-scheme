# modal-scheme
A modal reconstruction of Scheme by way of CBPV

For reasons of understanding/interoperability/implementation it is
useful to "explain" untyped languages by translation to a typed
language that implements the single type as a complex recursive type.
It's quite easy to do this for Scheme if we are willing to do things
like encode multiple arguments and multiple return values as lists and
encode continuations by CPS, but this is not reflective of how these
features are implemented or understood intuitively by programmers,
which are as manipulating the *stack* directly.

To model these, we should make a typed language that takes the stack
seriously, and that language is Levy's Call-by-push-value (CBPV). Out
of the box, CBPV already models the stack-based calling convention for
multiple argument functions. We further modify it by adding a kind of
*positive* computation type to model multiple return values.

The result is a modal typed language in which constructs like "dot
args" for variable arity, case-lambda, values and call-with-values are
all decomposed into recursive usages of simpler building-blocks.

I haven't explored (delimited or undelimited) continuations yet, but
we'll see.
