type opts_t = { ansi : bool }

let rec parse_args opts args =
  match args with
  | "-ansi" :: args -> parse_args { ansi = true } args
  | "-help" :: _ ->
      print_endline "Usage:";
      print_endline "  -ansi  enable ansi colors";
      print_endline "  -help  show this help message";
      exit 1
  | [] -> opts
  | arg :: _ ->
      print_endline
        ("Invalid arg: \"" ^ arg ^ "\". Use \"-help\" for usage info.");
      exit 1

let opts =
  let args = Sys.argv |> Array.to_seq |> Seq.drop 1 |> List.of_seq in
  parse_args { ansi = false } args

let rec print_lines lines =
  match lines with
  | line :: lines ->
      let fmt_line =
        if opts.ansi then
          line |> String.to_seq
          |> Seq.flat_map (function
               | '<' -> "\x1b[37;1m" |> String.to_seq
               | '>' -> "\x1b[0m" |> String.to_seq
               | c -> Seq.return c)
          |> String.of_seq
        else
          line |> String.to_seq
          |> Seq.filter (fun c -> c != '<' && c != '>')
          |> String.of_seq
      in
      print_string fmt_line;
      print_newline ();
      print_lines lines
  | [] -> ()

let help_msg () =
  print_lines
    [
      "Help:";
      "  - Identifiers are <[a-zA-Z][0-9]*> or <{[a-zA-Z0-9_]+}>";
      "    E.g. <\"a12\"> <\"{long_name}\">, <\"c\">";
      "    <\"abc\"> is three identifiers, not one";
      "  - Whitespace is completely ignored";
      "    [Ex] To define church encoding numerals:";
      "      <n0 = \\fx.x>             - zero";
      "      <n1 = \\fx.fx>            - one";
      "      <n2 = \\fx.f(fx)>         - two";
      "      <n3 = \\fx.f(f(fx))>      - three";
      "  - Use <\"=\"> as a shorthand for defining top-level functions";
      "    [Ex] To define succ, add, mul:";
      "      <{succ}n = \\fx.f(nfx)>";
      "      <{add}mn = \\fx.mf(nfx)>";
      "      <{mul}mn = \\fx.m(nf)x>";
      "  - You can use previously defined things:";
      "      <{add}(n1)({mul} n2 n3)  - seven";
      "  - Lambda expressions go all the way right";
      "      <(\\f.f)\\y.y>    - <\\y.y>";
      "      <\\f.f\\y.y>      - <\\y.y>";
      "  - Certain expressions don't work";
      "      <\\f.(\\x.f(xx))(\\x.f(xx))>";
      "      <(\\x.xx)(\\x.xx)>";
    ]

let ansi_fmt i s =
  match i with
  | i when i < 0 -> "\x1b[37;1m" ^ s ^ "\x1b[0m"
  | i when i > 0 ->
      let color_num =
        List.nth
          [ 176; 140; 104; 110; 116; 115; 114; 150; 186; 180; 174; 175 ]
          ((i - 1) mod 12)
      in
      Printf.sprintf "\x1b[38;5;%dm%s\x1b[0m" color_num s
  | _ -> s

let handle_expr env input =
  match Lambda_calc.Parse.expr input with
  | Some e ->
      Readline.add_history input;
      let e' = Lambda_calc.Interp.eval env e in
      let str =
        Lambda_calc.Parse.(if opts.ansi then repr_ex ansi_fmt e' else repr e)
      in
      print_string "   ";
      print_endline str;
      Some env
  | None -> None

let handle_stmt env input =
  match Lambda_calc.Parse.stmt input with
  | Some (ident, e) ->
      Readline.add_history input;
      let e' = Lambda_calc.Interp.eval env e in
      let str =
        Lambda_calc.Parse.(if opts.ansi then repr_ex ansi_fmt e' else repr e)
      in
      print_string "   ";
      if opts.ansi then print_string "\x1b[37m";
      print_string ident;
      if opts.ansi then print_string "\x1b[0m";
      print_char '=';
      print_endline str;
      Some ((ident, e') :: env)
  | None -> None

let rec repl env =
  let input =
    let prompt = if opts.ansi then " \x1b[32m<λ>\x1b[0m " else " <λ> " in
    try Readline.readline ~prompt () with Stdlib.Sys.Break -> Some ""
  in
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
            let res =
              if String.contains input '=' then handle_stmt env input
              else handle_expr env input
            in
            match res with
            | Some env -> env
            | None ->
                print_string "   ";
                if opts.ansi then print_string "\x1b[31m";
                print_string "Parse Error";
                if opts.ansi then print_string "\x1b[0m";
                print_newline ();
                env
          in
          repl env)
  | None -> ()
;;

if opts.ansi then print_string "\x1b[1m";
print_endline "  ~~~~~ Lambda Calculus Interpreter ~~~~~";
print_newline ();
print_endline "     ('!quit' - leave, '!help' - help)";
if opts.ansi then print_string "\x1b[0m";
print_newline ();
Readline.init ~catch_break:true ();
repl []
