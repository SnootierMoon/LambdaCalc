# LambdaCalc
A call-by-name lambda calculus interpreter in OCaml

Run with `ocaml lcalc.ml`.

For readline support, install `rlwrap` or something similar, and run `rlwrap
ocaml lcalc.ml`.

For ANSI colors, run `ocaml lcalc.ml -ansi` or `rlwrap -A ocaml lcalc.ml
-ansi`.

You can run `ocaml lcalc.ml -xfac.lc -xcomb.lc -ansi` to start off with a few
common functions. Enter `!` to see what functions you have.

The call-by-value evaluator is based on
[this](https://www.cl.cam.ac.uk/~lp15/MLbook/PDF/chapter9.pdf).

# Screenshots

![screenshot](res/scrot.png)

This screenshot above demonstrates:

 * the church encoding of 0-4, where 1 is eta-reduced to the identity function,
 * the successor, add, and multiply operations, written in functional style,
 * the S and K combinators,
 * the add function expressed with S and K combinators, demonstrating
   alpha-conversion to prevent capture of variables.

# TODO

 * Call-by-need, memoization of head/beta normal form calculation of bound
   variables in lazy evaluator (e.g. `(\\x.xx)((\\a.a)y)` performs 3 beta
   reductions when it can be two)
   * `lazy_memo` branch
 * Cleaner handling of free vars & env in eval logic
 * Ability to reference previous expressions with #[number] identifiers 
