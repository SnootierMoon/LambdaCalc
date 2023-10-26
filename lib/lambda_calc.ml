type expr_t =
  | BVar of int (* A bound variable, ID'd by its De-Brujin index *)
  | FVar of string (* A free variable *)
  | App of expr_t * expr_t (* A function application *)
  | Abs of string * expr_t (* A function abstraction *)

(*

EBNF for Lambda Calculus:

    <input> ::= 
          <expr> <ws>
        | <stmt> <ws>
        | <ws>
  
    <expr> ::=
          <lambda_expr>
        | (<closed_expr>)+ (<lambda_expr>)?
  
    <lambda_expr> ::=
          <ws> "\\" <args> <ws> "." <expr>
  
    <closed_expr> ::=
          <ws> <ident>                                              
        | <ws> "(" <expr> <ws> ")"
  
    <stmt> ::= <ws> <ident> <args> <ws> "=" <expr>
  
    <args> ::= (<ws> <ident>)*         
  
    <ident> ::= 
            ([a-z] | [A-Z]) [0-9]*
          | "{" ([a-z] | [A-Z] | [0-9] | "_")+ "}"             
  
    <ws> ::= (" ")*

*)

module type ParseType = sig
  val expr : string -> expr_t option
  val stmt : string -> (string * expr_t) option
  val repr : expr_t -> string
end

module Parse : ParseType = struct
  (* remove spaces from the beginning of a char list *)
  let rec skip_space cs = match cs with ' ' :: cs -> skip_space cs | cs -> cs

  (* read an identifier from the beginning of a char list *)
  let read_ident cs =
    let rec loop_simple ident cs =
      match cs with
      | ('0' .. '9' as c) :: cs -> loop_simple (fun rst -> ident (c :: rst)) cs
      | cs -> Some (ident [] |> List.to_seq |> String.of_seq, cs)
    in
    let rec loop_escaped ident cs =
      match cs with
      | (('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c) :: cs ->
          loop_escaped (fun rst -> ident (c :: rst)) cs
      | '}' :: cs -> Some (ident [] |> List.to_seq |> String.of_seq, cs)
      | _ -> None
    in
    match cs with
    | (('a' .. 'z' | 'A' .. 'Z') as c) :: cs ->
        loop_simple (fun rst -> c :: rst) cs
    | '{' :: cs -> loop_escaped Fun.id cs
    | _ -> None

  (* read an expression from the beginning of a char list *)
  let rec read_expr bvars cs =
    let rec loop el cs =
      let cs = skip_space cs in
      match read_abs bvars cs with
      | Some (er, cs) -> Some (App (el, er), cs)
      | None -> (
          match read_atom bvars cs with
          | Some (er, cs) -> loop (App (el, er)) cs
          | None -> Some (el, cs))
    in
    let cs = skip_space cs in
    match read_abs bvars cs with
    | Some (e, cs) -> Some (e, cs)
    | None -> (
        match read_atom bvars cs with Some (e, cs) -> loop e cs | None -> None)

  (* read a lambda abstraction from the beginning of a char list *)
  and read_abs bvars cs =
    let rec loop params bvars cs =
      match skip_space cs with
      | '.' :: cs -> Some (params, bvars, cs)
      | cs -> (
          match read_ident cs with
          | Some (ident, cs) ->
              loop (fun rst -> params (Abs (ident, rst))) (ident :: bvars) cs
          | None -> None)
    in
    match cs with
    | '\\' :: cs -> (
        match cs |> skip_space |> read_ident with
        | Some (ident, cs) -> (
            match loop (fun e -> Abs (ident, e)) (ident :: bvars) cs with
            | Some (params, bvars, cs) -> (
                match read_expr bvars cs with
                | Some (e, cs) -> Some (params e, cs)
                | None -> None)
            | None -> None)
        | None -> None)
    | _ -> None

  (* read a parenthesized expression or a variable, aka <closed_expr>, from the
     beggining of a char list *)
  and read_atom bvars cs =
    match cs with
    | '(' :: cs -> (
        match read_expr bvars cs with
        | Some (e, cs) -> (
            match skip_space cs with ')' :: cs -> Some (e, cs) | _ -> None)
        | None -> None)
    | cs -> (
        match read_ident cs with
        | Some (ident, cs) ->
            let var =
              bvars
              |> List.mapi (fun idx bvar -> (idx, bvar))
              |> List.find_map (fun (idx, bvar) ->
                     if ident = bvar then Some idx else None)
              |> Option.fold ~none:(FVar ident) ~some:(fun idx -> BVar idx)
            in
            Some (var, cs)
        | None -> None)

  (* parse some input as an expression *)
  let expr str =
    let cs = str |> String.to_seq |> List.of_seq in
    match read_expr [] cs with
    | Some (e, cs) when skip_space cs = [] -> Some e
    | _ -> None

  (* parse some input as a statement (function definition) *)
  let stmt str =
    let rec read_args args bvars cs =
      match skip_space cs with
      | '=' :: cs -> Some (args, bvars, cs)
      | cs -> (
          match read_ident cs with
          | Some (ident, cs) ->
              read_args (fun e -> args (Abs (ident, e))) (ident :: bvars) cs
          | None -> None)
    in
    let cs = str |> String.to_seq |> List.of_seq in
    match cs |> skip_space |> read_ident with
    | Some (name, cs) -> (
        match read_args Fun.id [] cs with
        | Some (args, bvars, cs) -> (
            match read_expr bvars cs with
            | Some (e, cs) when skip_space cs = [] -> Some (name, args e)
            | _ -> None)
        | None -> None)
    | None -> None

  (* serialize an expression into a string, using the minimal number of
     parenthese, with the property that
       forall e: expr_t . e |> Parser.repr |> Parser.expr = Some e *)
  let repr e =
    let simple_ident ident =
      match ident.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' ->
          String.for_all
            (function '0' .. '9' -> true | _ -> false)
            (String.sub ident 1 (String.length ident - 1))
      | _ -> false
    in
    let dot abs repr = if abs then fun rst -> '.' :: repr rst else repr in
    let rec dfs abs bvars e =
      match e with
      | BVar idx ->
          let ident = List.nth bvars idx in
          let ident = if simple_ident ident then ident else "{" ^ ident ^ "}" in
          let rep = dot abs (String.fold_right (fun c l -> c :: l) ident) in
          (false, false, rep)
      | FVar ident ->
          let ident = if simple_ident ident then ident else "{" ^ ident ^ "}" in
          let rep = dot abs (String.fold_right (fun c l -> c :: l) ident) in
          (false, false, rep)
      | App (el, er) ->
          let open_l, _, repr_l = dfs false bvars el in
          let open_r, app_r, repr_r = dfs false bvars er in
          if open_l then
            if app_r then
              let rep =
                dot abs (fun rst ->
                    '(' :: repr_l (')' :: '(' :: repr_r (')' :: rst)))
              in
              (false, true, rep)
            else
              let rep =
                dot abs (fun rst -> '(' :: repr_l (')' :: repr_r rst))
              in
              (open_r, true, rep)
          else if app_r then
            let rep =
              dot abs (fun rst -> repr_l ('(' :: repr_r (')' :: rst)))
            in
            (false, true, rep)
          else (open_r, true, dot abs (fun rst -> repr_l (repr_r rst)))
      | Abs (ident, e) ->
          let _, _, repr_in = dfs true (ident :: bvars) e in
          let rep =
            if abs then fun rst ->
              String.fold_right (fun c l -> c :: l) ident (repr_in rst)
            else fun rst ->
              '\\' :: String.fold_right (fun c l -> c :: l) ident (repr_in rst)
          in
          (true, false, rep)
    in
    let _, _, rep = dfs false [] e in
    rep [] |> List.to_seq |> String.of_seq
end

module type InterpType = sig
  val eval : (string * expr_t) list -> expr_t -> expr_t
  val repl : unit -> unit
end

module Interp = struct
  (* evaluate a lambda calculus expression *)
  let eval env e =
    let rec shiftr i e =
      match e with
      | BVar idx when idx >= i -> BVar (idx + 1)
      | BVar idx -> BVar idx
      | FVar ident -> FVar ident
      | App (el, er) -> App (shiftr i el, shiftr i er)
      | Abs (ident, e) -> Abs (ident, shiftr (i + 1) e)
    in
    let rec subst i e' e =
      match e with
      | BVar idx when idx > i -> BVar (idx - 1)
      | BVar idx when idx = i -> e'
      | BVar idx -> BVar idx
      | FVar ident -> FVar ident
      | App (el, er) -> App (subst i e' el, subst i e' er)
      | Abs (ident, e) -> Abs (ident, subst (i + 1) (shiftr 0 e') e)
    in
    let rec shiftl i e =
      match e with
      | BVar idx when idx > i -> Some (BVar (idx - 1))
      | BVar idx when idx = i -> None
      | BVar idx -> Some (BVar idx)
      | FVar ident -> Some (FVar ident)
      | App (el, er) -> (
          match (shiftl i el, shiftl i er) with
          | Some el, Some er -> Some (App (el, er))
          | _, _ -> None)
      | Abs (ident, e) -> (
          match shiftl i e with Some e -> Some (Abs (ident, e)) | None -> None)
    in
    let rec dfs e =
      match e with
      | BVar idx -> BVar idx
      | FVar ident -> (
          match List.assoc_opt ident env with Some e -> e | None -> FVar ident)
      | App (el, er) -> (
          match (dfs el, dfs er) with
          | Abs (_, el), er -> dfs (subst 0 er el)
          | el, er -> App (el, er))
      | Abs (ident, e) -> (
          match dfs e with
          | App (e, BVar 0) -> (
              match shiftl 0 e with
              | Some e -> e
              | None -> Abs (ident, App (e, BVar 0)))
          | e -> Abs (ident, e))
    in

    let fix_idents e =
      let mk_new_ident ident =
        let rec loop hd tl =
          let curr = hd.[String.length hd - 1] in
          let hd = String.sub hd 0 (String.length hd - 1) in
          match curr with
          | '0' .. '8' ->
              let inc_curr =
                String.make 1 (char_of_int (1 + int_of_char curr))
              in
              hd ^ inc_curr ^ tl
          | '9' -> loop hd ("0" ^ tl)
          | curr -> hd ^ String.make 1 curr ^ "1" ^ tl
        in
        loop ident ""
      in
      let mk_good_ident min_depth sub_idents ident =
        let is_bad ident =
          List.exists
            (fun (depth, sub_ident) -> sub_ident = ident && depth < min_depth)
            sub_idents
        in
        let rec loop ident =
          if is_bad ident then loop (mk_new_ident ident) else ident
        in
        loop ident
      in
      let rec dfs_get_idents bvars e =
        match e with
        | BVar idx -> [ (List.length bvars - idx, List.nth bvars idx) ]
        | FVar ident -> [ (Int.min_int, ident) ]
        | App (el, er) -> dfs_get_idents bvars el @ dfs_get_idents bvars er
        | Abs (ident, e) -> dfs_get_idents (ident :: bvars) e
      in
      let rec dfs bvars e =
        match e with
        | BVar idx -> BVar idx
        | FVar ident -> FVar ident
        | App (el, er) -> App (dfs bvars el, dfs bvars er)
        | Abs (ident, e) ->
            let sub_idents = dfs_get_idents (ident :: bvars) e in
            let min_depth = List.length bvars + 1 in
            let ident = mk_good_ident min_depth sub_idents ident in
            let e = dfs (ident :: bvars) e in
            Abs (ident, e)
      in
      dfs [] e
    in
    e |> dfs |> fix_idents
end
