# LambdaCalc
A lambda calculus interpreter in OCaml

Requires: ocaml 5.1, dune, readline

Run with `dune exec repl`.

# Screenshots

![screenshot](res/scrot.png)

This screenshot above demonstrates:

 * the church encoding of 0-4, where 1 is simplified to the identity function,
 * the successor, add, and multiply operations, written in functional style,
 * the S and K combinators,
 * the add function expressed with S and K combinators, demonstrating
   alpha-conversion to resolve conflicting bound identifiers.

This program does not handle certain expressions, such as expressions that can
be beta-reduced infinite times like `(\x.xx)\x.xx` and `\f.(\x.f(xx))\x.f(xx)`.
