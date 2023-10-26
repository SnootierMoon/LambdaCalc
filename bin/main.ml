type opts_t = { ansi : bool; max_depth: int; max_beta: int }

let rec parse_args opts args =
  match args with
  | "-ansi" :: args -> parse_args { opts with ansi = true } args
  | "-help" :: _ ->
      print_endline "Usage:";
      print_endline "  -ansi  enable ansi colors";
      print_endline "  -help  show this help message";
      print_endline "  -dN    set the max output depth to N, measured in abstractions";
      print_endline "  -nN    set the max beta reductions per depth to N";
      exit 1
  | arg :: args when String.starts_with ~prefix:"-d" arg ->
      let ds = String.sub arg 2 (String.length arg - 2) in
      (match int_of_string_opt ds with
      | Some d -> parse_args { opts with max_depth = d } args
      | None ->
                      print_endline ("Invalid int: \"" ^ ds ^ "\".");
                      exit 1)
  | arg :: args when String.starts_with ~prefix:"-n" arg ->
      let ns = String.sub arg 2 (String.length arg - 2) in
      (match int_of_string_opt ns with
      | Some n -> parse_args { opts with max_depth = n } args
      | None ->
                      print_endline ("Invalid int: \"" ^ ns ^ "\".");
                      exit 1)
  | [] -> opts
  | arg :: _ ->
      print_endline
        ("Invalid arg: \"" ^ arg ^ "\". Use \"-help\" for usage info.");
      exit 1

let opts =
  let args = Sys.argv |> Array.to_seq |> Seq.drop 1 |> List.of_seq in
  parse_args { ansi = false; max_depth = 1000; max_beta = 1000; } args

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

type err_t = ParseError | EvalError

let handle_expr env input =
  match Lambda_calc.Parse.expr input with
  | Some e ->
      Readline.add_history input;
      (match Lambda_calc.Interp.eval ~max_depth:opts.max_depth ~max_beta:opts.max_beta env e with
      | Some e' ->
      let str =
        Lambda_calc.Parse.(if opts.ansi then repr_ex ansi_fmt e' else repr e)
      in
      print_string "   ";
      print_endline str;
      Ok env
      | None -> Error EvalError)
  | None -> Error ParseError

let handle_stmt env input =
  match Lambda_calc.Parse.stmt input with
  | Some (ident, e) ->
      Readline.add_history input;
      (match Lambda_calc.Interp.eval ~max_depth:opts.max_depth ~max_beta:opts.max_beta env e with
      | Some e' -> let str =
        Lambda_calc.Parse.(if opts.ansi then repr_ex ansi_fmt e' else repr e)
      in
      print_string "   ";
      if opts.ansi then print_string "\x1b[37m";
      print_string ident;
      if opts.ansi then print_string "\x1b[0m";
      print_char '=';
      print_endline str;
      Ok ((ident, e') :: env)
      | None -> Error EvalError)
  | None -> Error ParseError

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
            | Ok env -> env
            | Error ParseError ->
                print_string "   ";
                if opts.ansi then print_string "\x1b[31m";
                print_string "Parse Error";
                if opts.ansi then print_string "\x1b[0m";
                print_newline ();
                env
            | Error EvalError ->
                print_string "   ";
                if opts.ansi then print_string "\x1b[31m";
                print_string "Eval Error";
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
