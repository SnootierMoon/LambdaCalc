let rec print_lines lines =
  match lines with
  | line :: lines ->
      print_string line;
      print_newline ();
      print_lines lines
  | [] -> ()

let help_msg () =
  print_lines
    [
      "\x1b[37m  Help:";
      "  - Identifiers are [a-zA-Z][0-9]* or {[a-zA-Z0-9_]+}";
      "    E.g. \"a12\" \"{long_name}\", \"c\"";
      "    \"abc\" is three identifiers, not one";
      "  - Whitespace is completely ignored";
      "    [Ex] To define church encoding numerals:";
      "      n0 = \\fx.x             - zero";
      "      n1 = \\fx.fx            - one";
      "      n2 = \\fx.f(fx)         - two";
      "      n3 = \\fx.f(f(fx))      - three";
      "  - Use \"=\" as a shorthand for defining top-level functions";
      "    [Ex] To define succ, add, mul:";
      "      {succ}n = \\fx.f(nfx)";
      "      {add}mn = \\fx.mf(nfx)";
      "      {mul}mn = \\fx.m(nf)x";
      "  - You can use previously defined things:";
      "      {add}(n1)({mul} n2 n3)  - seven";
      "  - Lambda expressions go all the way right";
      "      (\\f.f)\\y.y ~~~>  \\y.y";
      "      \\f.f\\y.y   ~/~>  \\y.y";
      "  - Certain expressions don't work";
      "      \\f.(\\x.f(xx))(\\x.f(xx))  ~~~> stack overflow";
      "      (\\x.xx)(\\x.xx) ~~~> freezes\x1b[0m";
    ]

let handle_expr env input =
  match Lambda_calc.Parse.expr input with
  | Some e ->
      Readline.add_history input;
      let e' = Lambda_calc.Interp.eval env e in
      let str = Lambda_calc.Parse.repr e' in
      print_endline ("\x1b[37m  " ^ str ^ "\x1b[0m");
      env
  | _ ->
      print_endline "  \x1b[31mParse Error\x1b[0m";
      env

let handle_stmt env input =
  match Lambda_calc.Parse.stmt input with
  | Some (ident, e) ->
      Readline.add_history input;
      let e' = Lambda_calc.Interp.eval env e in
      let str = Lambda_calc.Parse.repr e' in
      print_endline ("\x1b[37m  " ^ ident ^ "=" ^ str ^ "\x1b[0m");
      (ident, e') :: env
  | _ ->
      print_endline "  \x1b[31mParse Error\x1b[0m";
      env

let rec repl env =
  let input = Readline.readline ~prompt:"\x1b[32m<λ>\x1b[0m " () in
  match input with
  | Some input -> (
      match input |> String.trim |> String.lowercase_ascii with
      | "!quit" -> ()
      | "!help" ->
          help_msg ();
          repl env
      | "" -> repl env
      | _ ->
          let env =
            if String.contains input '=' then handle_stmt env input
            else handle_expr env input
          in
          repl env)
  | None -> ()
;;

Readline.init ();
print_endline "\x1b[1;37m  ~~~~~ Lambda Calculus Interpreter ~~~~~";
print_newline ();
print_endline "     ('!quit' - leave, '!help' - help)\x1b[0m";
print_newline ();
repl []
