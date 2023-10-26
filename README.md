# LambdaCalc
A call-by-value lambda calculus interpreter in OCaml

Requires: ocaml 5.1, dune, readline

Run with `dune exec repl`.

 * If you are in CMSC330 @ UMD, you will have to run the following commands
   to install OCaml 5.1 without uninstalling OCaml 4.13:
   * `opam switch create 5.1.0` - install 5.1.0
   * `opam switch set 5.1.0` - enable 5.1.0 temporarily
   * `eval $(opam env)` - add 5.1.0 env vars, such as PATH
   * `opam install dune readline` - install dependencies in 5.1.0
   * to go back to 4.13 for projects, do `opam switch set default`

# Screenshots

![screenshot](res/scrot.png)

This screenshot above demonstrates:

 * the church encoding of 0-4, where 1 is simplified to the identity function,
 * the successor, add, and multiply operations, written in functional style,
 * the S and K combinators,
 * the add function expressed with S and K combinators, demonstrating
   alpha-conversion to resolve conflicting bound identifiers.

This program does not handle certain expressions, such as expressions that can
be beta-reduced infinitely many times, like `(\x.xx)\x.xx` and
`\f.(\x.f(xx))\x.f(xx)`.
