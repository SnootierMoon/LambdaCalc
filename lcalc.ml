module Util = struct
  let suffix n str = String.sub str n (String.length str - n)
end

type expr_t =
  (* bound variable, identified by De Brujin index *)
  | BVar of int
  (* free variable *)
  | FVar of string
  (* application *)
  | App of expr_t * expr_t
  (* abstraction *)
  | Abs of string * expr_t

type fmt_punct_t = LParen | RParen | Lambda | Dot
type fmt_t = PunctF of fmt_punct_t | BVarF of int * string | FVarF of string

(*

EBNF for Lambda Calculus:

    <input> ::= 
          ("*")? <stmt> <ws>
        | ("*")? <expr> <ws>
        | <ws>
  
    <stmt> ::= <ws> <ident> <args> <ws> "=" <expr>
  
    <expr> ::=
          <lambda_expr>
        | (<closed_expr>)+ (<lambda_expr>)?
  
    <lambda_expr> ::=
          <ws> ("\\" | "λ") <args> <ws> "." <expr>
  
    <closed_expr> ::=
          <ws> <ident>                                              
        | <ws> "(" <expr> <ws> ")"
  
    <args> ::= (<ws> <ident>)*         
  
    <ident> ::= 
        ([A-Z] | [a-z]) ([A-Z] | [a-z] | [0-9])*
        | "{" ([a-z] | [A-Z] | [0-9] | "_")+ "}"             
  
    <ws> ::= (" ")*

*)

module type ParseSig = sig
  val expr : string -> expr_t option
  val stmt : string -> (string * expr_t) option
  val repr_ex : (fmt_t -> string) -> expr_t -> string
  val repr : expr_t -> string
end

module Parse = struct
  (* parser for <ws> *)
  let rec read_ws cs = match cs with ' ' :: cs -> read_ws cs | cs -> cs

  (* parser for <ident> *)
  let rec read_ident cs =
    let rec read_lowercase_alnum ident cs =
      match cs with
      | ('0' .. '9' as c) :: cs ->
          read_lowercase_alnum (fun rst -> ident (c :: rst)) cs
      | cs -> Some (ident, cs)
    in
    let rec read_alnum ident cs =
      match cs with
      | (('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c) :: cs ->
          read_alnum (fun rst -> ident (c :: rst)) cs
      | '}' :: cs -> Some (ident, cs)
      | _ -> None
    in
    match
      match cs with
      | (('A' .. 'Z' | 'a' .. 'z') as c) :: cs ->
          read_lowercase_alnum (fun rst -> c :: rst) cs
      | '{' :: cs -> read_alnum Fun.id cs
      | _ -> None
    with
    | Some (ident, cs) -> Some (ident [] |> List.to_seq |> String.of_seq, cs)
    | None -> None

  (* parser for <closed_expr> *)
  let rec read_closed_expr bvars cs =
    match cs with
    | '(' :: cs -> (
        match read_expr bvars cs with
        | Some (expr, cs) -> (
            match read_ws cs with ')' :: cs -> Some (expr, cs) | _ -> None)
        | None -> None)
    | cs -> (
        match read_ident cs with
        | Some (ident, cs) ->
            let var =
              bvars
              |> List.mapi (fun idx bvar -> (idx, bvar))
              |> List.find_map (function
                   | idx, bvar when ident = bvar -> Some idx
                   | _ -> None)
              |> Option.fold ~none:(FVar ident) ~some:(fun idx -> BVar idx)
            in
            Some (var, cs)
        | None -> None)

  (* parser for <lambda_expr> *)
  and read_lambda_expr bvars cs =
    let rec read_params params bvars cs =
      match read_ws cs with
      | '.' :: cs -> Some (params, bvars, cs)
      | cs -> (
          match read_ident cs with
          | Some (ident, cs) ->
              read_params
                (fun rst -> params (Abs (ident, rst)))
                (ident :: bvars) cs
          | None -> None)
    in
    match cs with
    | '\\' :: cs | '\206' :: '\187' :: cs -> (
        match cs |> read_ws |> read_ident with
        | Some (ident, cs) -> (
            match read_params (fun e -> Abs (ident, e)) (ident :: bvars) cs with
            | Some (params, bvars, cs) -> (
                match read_expr bvars cs with
                | Some (expr, cs) -> Some (params expr, cs)
                | None -> None)
            | None -> None)
        | None -> None)
    | _ -> None

  (* parser for <expr> *)
  and read_expr bvars cs =
    let rec read_applied_exprs app cs =
      let cs = read_ws cs in
      match read_lambda_expr bvars cs with
      | Some (expr, cs) -> Some (App (app, expr), cs)
      | None -> (
          match read_closed_expr bvars cs with
          | Some (expr, cs) -> read_applied_exprs (App (app, expr)) cs
          | None -> Some (app, cs))
    in
    let cs = read_ws cs in
    match read_lambda_expr bvars cs with
    | Some (expr, cs) -> Some (expr, cs)
    | None -> (
        match read_closed_expr bvars cs with
        | Some (expr, cs) -> read_applied_exprs expr cs
        | None -> None)

  (* parse an expr in a string *)
  let expr str =
    let cs = str |> String.to_seq |> List.of_seq in
    match read_expr [] cs with
    | Some (expr, cs) when read_ws cs = [] -> Some expr
    | _ -> None

  (* parse a stmt in a string *)
  let stmt str =
    let rec read_params params bvars cs =
      match read_ws cs with
      | '=' :: cs -> Some (params, bvars, cs)
      | cs -> (
          match read_ident cs with
          | Some (ident, cs) ->
              read_params
                (fun expr -> params (Abs (ident, expr)))
                (ident :: bvars) cs
          | None -> None)
    in
    let cs = str |> String.to_seq |> List.of_seq in
    match cs |> read_ws |> read_ident with
    | Some (name, cs) -> (
        match read_params Fun.id [] cs with
        | Some (params, bvars, cs) -> (
            match read_expr bvars cs with
            | Some (expr, cs) when read_ws cs = [] -> Some (name, params expr)
            | _ -> None)
        | None -> None)
    | None -> None

  (* serialize an expression into a string, using the minimal number of
     parentheses.
     The serialization should satisfy the property that, for e: expr_t
     where e has no captured variables:
       e |> Parse.repr |> Parse.expr = Some e *)
  let repr_ex fmt expr =
    let string_to_dlist = String.fold_right (fun c rst -> c :: rst) in
    let escape_ident ident =
      if
        match ident.[0] with
        | 'a' .. 'z' | 'A' .. 'Z' ->
            String.for_all
              (function '0' .. '9' -> true | _ -> false)
              (Util.suffix 1 ident)
        | _ -> false
      then ident
      else "{" ^ ident ^ "}"
    in
    let l_paren = fmt (PunctF LParen) |> String.to_seq |> List.of_seq in
    let r_paren = fmt (PunctF RParen) |> String.to_seq |> List.of_seq in
    let lambda = fmt (PunctF Lambda) |> String.to_seq |> List.of_seq in
    let dot = fmt (PunctF Dot) |> String.to_seq |> List.of_seq in
    let rec dfs is_in_abs bvars expr =
      match expr with
      | BVar idx ->
          let ident =
            match List.nth_opt bvars idx with Some s -> s | None -> "?"
          in
          let fmt = fmt (BVarF (List.length bvars - idx, escape_ident ident)) in
          (false, string_to_dlist fmt)
      | FVar ident ->
          let fmt = fmt (FVarF (escape_ident ident)) in
          (false, string_to_dlist fmt)
      | App (l_expr, r_expr) ->
          let l_open, l_repr = dfs false bvars l_expr in
          let l_closed =
            if l_open then fun rst -> l_paren @ l_repr (r_paren @ rst)
            else l_repr
          in
          let r_open, r_repr = dfs false bvars r_expr in
          let r_app = match r_expr with App _ -> true | _ -> false in
          let r_closed =
            if r_app then fun rst -> l_paren @ r_repr (r_paren @ rst)
            else r_repr
          in
          let repr rst = l_closed (r_closed rst) in
          (r_open && not r_app, repr)
      | Abs (ident, expr) ->
          let _, inner_repr = dfs true (ident :: bvars) expr in
          let ident = fmt (BVarF (List.length bvars + 1, escape_ident ident)) in
          let inner_abs = match expr with Abs _ -> true | _ -> false in
          let dot_inner =
            if inner_abs then inner_repr else fun rst -> dot @ inner_repr rst
          in
          let lambda = if is_in_abs then Fun.id else ( @ ) lambda in
          let repr rst = lambda (string_to_dlist ident (dot_inner rst)) in
          (true, repr)
    in
    let _, repr = dfs false [] expr in
    repr [] |> List.to_seq |> String.of_seq

  let repr =
    repr_ex (function
      | PunctF LParen -> "("
      | PunctF RParen -> ")"
      | PunctF Lambda -> "λ"
      | PunctF Dot -> "."
      | BVarF (_, ident) | FVarF ident -> ident)
end

module type InterpSig = sig
  type env_t = Env of (string -> (env_t * expr_t) option)

  val fix_idents : expr_t -> expr_t
  val subst_fvars : env_t -> expr_t -> expr_t
  val eval : expr_t -> expr_t
  val eq : expr_t -> expr_t -> bool
end

module Interp : InterpSig = struct
  type env_t = Env of (string -> (env_t * expr_t) option)

  let fix_idents expr =
    let mk_new ident =
      let rec loop left_part right_part =
        let curr = left_part.[String.length left_part - 1] in
        let new_left_part = Util.suffix 1 left_part in
        match curr with
        | '0' .. '8' ->
            let inc_curr = String.make 1 (char_of_int (1 + int_of_char curr)) in
            new_left_part ^ inc_curr ^ right_part
        | '9' -> loop new_left_part ("0" ^ right_part)
        | _ -> left_part ^ "1" ^ right_part
      in
      loop ident ""
    in
    let is_good sub_idents depth ident =
      List.for_all
        (fun (sub_depth, sub_ident) ->
          (not (sub_ident = ident)) || sub_depth > depth)
        sub_idents
    in
    let rec mk_good sub_idents depth ident =
      if is_good sub_idents depth ident then ident
      else mk_good sub_idents depth (mk_new ident)
    in
    let rec get_idents bvars expr =
      match expr with
      | BVar idx -> [ (List.length bvars - idx, List.nth bvars idx) ]
      | FVar ident -> [ (Int.min_int, ident) ]
      | App (l_expr, r_expr) ->
          get_idents bvars l_expr @ get_idents bvars r_expr
      | Abs (ident, expr) -> get_idents (ident :: bvars) expr
    in
    let rec dfs bvars expr =
      match expr with
      | BVar idx -> BVar idx
      | FVar ident -> FVar ident
      | App (l_expr, r_expr) -> App (dfs bvars l_expr, dfs bvars r_expr)
      | Abs (ident, expr) ->
          let ident =
            mk_good (get_idents (ident :: bvars) expr) (List.length bvars) ident
          in
          Abs (ident, dfs (ident :: bvars) expr)
    in
    dfs [] expr

  let rec subst_fvars env expr =
    match expr with
    | BVar idx -> BVar idx
    | FVar ident ->
        let (Env env) = env in
        Option.fold (env ident)
          ~some:(fun (env, expr) -> subst_fvars env expr)
          ~none:(FVar ident)
    | App (l_expr, r_expr) ->
        App (subst_fvars env l_expr, subst_fvars env r_expr)
    | Abs (ident, expr) -> Abs (ident, subst_fvars env expr)

  let eval expr =
    let rec subst d u expr =
      let rec shift i d u =
        if i = 0 then u
        else
          match u with
          | BVar idx when idx >= d -> BVar (idx + i)
          | BVar idx -> BVar idx
          | FVar ident -> FVar ident
          | App (l_expr, r_expr) -> App (shift i d l_expr, shift i d r_expr)
          | Abs (ident, expr) -> Abs (ident, shift i (d + 1) expr)
      in
      match expr with
      | BVar idx when idx < d -> BVar idx
      | BVar idx when idx = d -> shift d 0 u
      | BVar idx -> BVar (idx - 1)
      | FVar ident -> FVar ident
      | App (l_expr, r_expr) -> App (subst d u l_expr, subst d u r_expr)
      | Abs (ident, expr) -> Abs (ident, subst (d + 1) u expr)
    in
    let rec head_nf expr =
      match expr with
      | BVar idx -> BVar idx
      | FVar ident -> FVar ident
      | App (l_expr, r_expr) -> (
          match head_nf l_expr with
          | Abs (ident, expr) -> head_nf (subst 0 r_expr expr)
          | expr -> App (expr, r_expr))
      | Abs (ident, expr) -> Abs (ident, head_nf expr)
    and by_name expr = args (head_nf expr)
    and args expr =
      match expr with
      | BVar idx -> BVar idx
      | FVar ident -> FVar ident
      | App (l_expr, r_expr) -> App (args l_expr, by_name r_expr)
      | Abs (ident, expr) -> Abs (ident, args expr)
    in
    let rec eta_reduce expr =
      let rec shiftl d expr =
        match expr with
        | BVar idx when idx > d -> Some (BVar (idx - 1))
        | BVar idx when idx = d -> None
        | BVar idx -> Some (BVar idx)
        | FVar ident -> Some (FVar ident)
        | App (l_expr, r_expr) -> (
            match (shiftl d l_expr, shiftl d r_expr) with
            | Some l_expr, Some r_expr -> Some (App (l_expr, r_expr))
            | _, _ -> None)
        | Abs (ident, expr) -> (
            match shiftl d expr with
            | Some expr -> Some (Abs (ident, expr))
            | None -> None)
      in
      match expr with
      | BVar idx -> BVar idx
      | FVar ident -> FVar ident
      | App (l_expr, r_expr) -> App (eta_reduce l_expr, eta_reduce r_expr)
      | Abs (ident, App (expr, BVar 0)) -> (
          match shiftl 0 expr with
          | Some expr -> eta_reduce expr
          | None -> Abs (ident, App (eta_reduce expr, BVar 0)))
      | Abs (ident, expr) -> Abs (ident, eta_reduce expr)
    in
    expr |> by_name |> eta_reduce

  let rec eq expr1 expr2 =
    match (expr1, expr2) with
    | BVar idx1, BVar idx2 -> idx1 = idx2
    | FVar ident1, FVar ident2 -> ident1 = ident2
    | App (l_expr1, r_expr1), App (l_expr2, r_expr2) ->
        eq l_expr1 l_expr2 && eq r_expr1 r_expr2
    | Abs (_, expr1), Abs (_, expr2) -> eq expr1 expr2
    | _ -> false
end

module Main = struct
  type opts_t = { ansi : bool; church_num : bool; church_bool : bool }

  exception Interrupt

  let ansi_fmt fmt =
    match fmt with
    | PunctF LParen -> "("
    | PunctF RParen -> ")"
    | PunctF Lambda -> "λ"
    | PunctF Dot -> "."
    | FVarF ident -> "\x1b[37;1m" ^ ident ^ "\x1b[0m"
    | BVarF (depth, ident) ->
        let color_num =
          List.nth
            [ 175; 176; 140; 104; 110; 116; 115; 114; 150; 186; 180; 174 ]
            (depth mod 12)
        in
        Printf.sprintf "\x1b[38;5;%dm%s\x1b[0m" color_num ident

  let rec print_lines opts lines =
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
        print_lines opts lines
    | [] -> ()

  let help_msg opts =
    print_lines opts
      [
        "Help:";
        "  - Identifiers are <[a-zA-Z][0-9]*> or <{[a-zA-Z0-9_]+}>";
        "    E.g. <\"a12\"> <\"{long_name}\">, <\"c\">";
        "    <\"abc\"> is three identifiers, not one";
        "    Whitespace is ignored (except for within an identifier)";
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
        "      <{add}(n1)({mul} n2 n3)>  - seven";
        "  - Lambda expressions go all the way right";
        "      <(\\f.f)\\y.y>    - <\\y.y>";
        "      <\\f.f\\y.y>      - <\\y.y>";
        "  - Expressions that don't have a beta-normal form will hang";
        "    [Ex]";
        "      <\\f.(\\x.f(xx))\\x.f(xx)>";
        "      <(\\x.xx)\\x.xx>";
        "    You can use still them with by putting <\"*\"> at the beginning";
        "    of a command, which disables evaluation:";
        "    [Ex]";
        "      <*Yf=(\\x.f(xx))\\x.f(xx)>";
        "  - Commands:";
        "    <\"!\">     - show environment";
        "    <\"!help\"> - show this help message";
        "    <\"!quit\"> - leave the session";
      ]

  let church_bools =
    [
      ("not", Abs ("x", App (App (BVar 0, FVar "false"), FVar "true")));
      ("and", Abs ("x", Abs ("y", App (App (BVar 1, BVar 0), FVar "false"))));
      ("or", Abs ("x", Abs ("y", App (App (BVar 1, FVar "true"), BVar 0))));
      ("false", Abs ("x", Abs ("y", BVar 0)));
      ("true", Abs ("x", Abs ("y", BVar 1)));
    ]

  let env_uniq opts env =
    let _, env =
      List.fold_left
        (fun (idents, env) (ident, expr) ->
          if List.mem ident idents then (idents, env)
          else (ident :: idents, (ident, expr) :: env))
        (if opts.church_bool then
           (List.map (fun (ident, _) -> ident) church_bools, church_bools)
         else ([], []))
        env
    in
    env

  let rec env_wrap opts env =
    let church_num x =
      let rec loop_app expr n =
        if n < x then loop_app (App (BVar 1, expr)) (n + 1) else expr
      in
      Abs ("f", Abs ("x", loop_app (BVar 0) 0))
    in
    let rec loop env ident =
      match env with
      | (ident', expr) :: env when ident' = ident ->
          Some (Interp.Env (loop env), expr)
      | _ :: env -> loop env ident
      | [] -> (
          match
            if opts.church_bool then List.assoc_opt ident church_bools else None
          with
          | Some expr -> Some (Interp.Env (fun ident -> None), expr)
          | None -> (
              match
                if opts.church_num then
                  ident |> int_of_string_opt |> Option.map church_num
                else None
              with
              | Some expr -> Some (Interp.Env (fun ident -> None), expr)
              | None -> None))
    in

    Interp.Env (loop env)

  let is_church_num expr =
    let rec loop expr i =
      match expr with
      | App (BVar 1, expr) -> loop expr (i + 1)
      | BVar 0 -> Some (string_of_int i)
      | _ -> None
    in
    match expr with
    | Abs (_, Abs (_, expr)) -> loop expr 0
    | Abs (_, BVar 0) -> Some "1"
    | _ -> None

  let print_names opts env expr =
    let names =
      env |> env_uniq opts
      |> List.filter_map (fun (ident, expr') ->
             if Interp.eq expr expr' then Some ident else None)
      |> List.map
           (if opts.ansi then fun ident -> "\x1b[37;1m" ^ ident ^ "\x1b[0m"
            else Fun.id)
      |> (if opts.church_num then fun names ->
            match is_church_num expr with
            | Some s when not (List.mem s names) -> s :: names
            | _ -> names
          else Fun.id)
      |> String.concat ", "
    in
    if not (names = "") then (
      print_string "   (";
      print_string names;
      print_endline ")")

  let print_env opts env =
    env |> env_uniq opts
    |> List.iter (fun (ident, expr) ->
           print_string "   ";
           if opts.ansi then print_string "\x1b[37;1m";
           print_string ident;
           if opts.ansi then print_string "\x1b[0m";
           print_char '=';
           let body =
             Parse.(if opts.ansi then repr_ex ansi_fmt else repr) expr
           in
           print_endline body)

  let rec repl opts env =
    let prompt = if opts.ansi then " \x1b[32m<λ>\x1b[0m " else " <λ> " in
    print_string prompt;
    try
      let input = read_line () in
      match input |> String.trim |> String.lowercase_ascii with
      | "!quit" -> ()
      | "!help" ->
          help_msg opts;
          repl opts env
      | "!" ->
          print_env opts env;
          repl opts env
      | cmd when String.starts_with ~prefix:"!" cmd ->
          Printf.printf "Invalid command: \"%s\"" (Util.suffix 1 cmd);
          print_newline ();
          repl opts env
      | "" -> repl opts env
      | _ -> (
          match
            let input, eval =
              if input.[0] = '*' then (Util.suffix 1 input, false)
              else (input, true)
            in
            if String.contains input '=' then
              match Parse.stmt input with
              | Some (ident, expr) ->
                  let expr =
                    if eval then
                      expr
                      |> Interp.subst_fvars (env_wrap opts env)
                      |> Interp.eval
                    else expr
                  in
                  let str =
                    Parse.(if opts.ansi then repr_ex ansi_fmt else repr) expr
                  in
                  print_string "   ";
                  if opts.ansi then print_string "\x1b[37;1m";
                  print_string ident;
                  if opts.ansi then print_string "\x1b[0m";
                  print_char '=';
                  print_endline str;
                  Some ((ident, expr) :: env)
              | None -> None
            else
              match Parse.expr input with
              | Some expr ->
                  let expr =
                    if eval then
                      expr
                      |> Interp.subst_fvars (env_wrap opts env)
                      |> Interp.eval
                    else expr
                  in
                  let str =
                    Parse.(if opts.ansi then repr_ex ansi_fmt else repr) expr
                  in
                  print_string "   ";
                  print_endline str;
                  print_names opts env expr;
                  Some env
              | None -> None
          with
          | Some env -> repl opts env
          | None ->
              print_string "   ";
              if opts.ansi then print_string "\x1b[31m";
              print_string "Parse Error";
              if opts.ansi then print_string "\x1b[0m";
              print_newline ();
              repl opts env)
    with
    | End_of_file -> print_newline ()
    | Interrupt ->
        print_newline ();
        repl opts env

  let read_file opts env filename =
    let rec read_lines i env channel =
      try
        let line = input_line channel |> String.trim in
        if line = "" then read_lines (i + 1) env channel
        else
          let line, eval =
            if line.[0] = '*' then (Util.suffix 1 line, false) else (line, true)
          in
          match Parse.stmt line with
          | Some (ident, expr) ->
              let expr =
                if eval then
                  expr |> Interp.subst_fvars (env_wrap opts env) |> Interp.eval
                else expr
              in
              read_lines (i + 1) ((ident, expr) :: env) channel
          | None ->
              Printf.printf "File \"%s\", line %d: Parse Error" filename i;
              print_newline ();
              exit 1
      with End_of_file -> env
    in
    let fs = open_in filename in
    let env = read_lines 1 env fs in
    close_in fs;
    env

  let rec parse_args env opts args =
    match args with
    | "-ansi" :: args -> parse_args env { opts with ansi = true } args
    | "-help" :: _ ->
        print_endline "Usage:";
        print_endline "  -ansi     enable ansi colors";
        print_endline "  -help     show this help message";
        print_endline "  -x[file]  include lambda calc statements from file";
        print_endline "  -num      include church numerals";
        print_endline "  -bool     include church booleans";
        exit 1
    | arg :: args when String.starts_with ~prefix:"-x" arg ->
        parse_args (read_file opts env (Util.suffix 2 arg)) opts args
    | "-bool" :: args -> parse_args env { opts with church_bool = true } args
    | "-num" :: args -> parse_args env { opts with church_num = true } args
    | [] -> (env, opts)
    | arg :: _ ->
        print_endline
          ("Invalid arg: \"" ^ arg ^ "\". Use \"-help\" for usage info.");
        exit 1

  let main () =
    Sys.set_signal Sys.sigint (Sys.Signal_handle (fun x -> raise Interrupt));
    let env, opts =
      let args = Sys.argv |> Array.to_seq |> Seq.drop 1 |> List.of_seq in
      parse_args []
        { ansi = false; church_num = false; church_bool = false }
        args
    in
    if opts.ansi then print_string "\x1b[1m";
    print_endline "  ~~~~~ Lambda Calculus Interpreter ~~~~~";
    print_newline ();
    print_endline "     ('!quit' - leave, '!help' - help)";
    if opts.ansi then print_string "\x1b[0m";
    print_newline ();
    repl opts env
end
;;

Main.main ()
