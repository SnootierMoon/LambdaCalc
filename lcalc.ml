module StringMap = Map.Make (String)

let suffix n str = String.sub str n (String.length str - n)

type expr_t =
  | BoundVar of int
  | FreeVar of string
  | Apply of expr_t * expr_t
  | Abstract of string * expr_t

type fmt_punct_t = LParen | RParen | Lambda | Dot

type fmt_t =
  | PunctFmt of fmt_punct_t
  | BoundVarFmt of int * string
  | FreeVarFmt of string

module type ParseSig = sig
  val expr : string -> (expr_t, int option) result
  val stmt : string -> (string * expr_t, int option) result
  val repr_ex : (fmt_t -> string) -> expr_t -> string
  val repr : expr_t -> string
end

module Parse : ParseSig = struct
  (* parser for <ws> *)
  let rec read_ws cis =
    match cis with (_, ' ') :: cis -> read_ws cis | cis -> cis

  (* option-parser for <ident> *)
  let rec read_ident var_position cis =
    let rec read_digits ident cis =
      match cis with
      | (_, ('0' .. '9' as c)) :: cis ->
          read_digits (fun rst -> ident (c :: rst)) cis
      | cis -> Ok (Some ident, cis)
    in
    let rec read_alnum ident cis =
      match cis with
      | (_, (('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c)) :: cis ->
          read_alnum (fun rst -> ident (c :: rst)) cis
      | (_, '}') :: cis -> Ok (Some ident, cis)
      | (i, _) :: _ -> Error (Some i)
      | [] -> Error None
    in
    match
      match cis with
      | (_, '#') :: cis when var_position ->
          read_digits (fun rst -> '#' :: rst) cis
      | (_, (('A' .. 'Z' | 'a' .. 'z') as c)) :: cis ->
          read_digits (fun rst -> c :: rst) cis
      | (_, '{')
        :: (_, (('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c))
        :: cis ->
          read_alnum (fun rst -> c :: rst) cis
      | (_, '{') :: (i, _) :: _ -> Error (Some i)
      | (_, '{') :: _ -> Error None
      | cis -> Ok (None, cis)
    with
    | Ok (Some ident, cis) ->
        Ok (Some (ident [] |> List.to_seq |> String.of_seq), cis)
    | Ok (None, cis) -> Ok (None, cis)
    | Error err -> Error err

  (* option-parser for <closed_expr> *)
  let rec read_closed_expr depth bvars cis =
    match cis with
    | (_, '(') :: cis -> (
        match read_expr depth bvars cis with
        | Ok (expr, cis) -> (
            match read_ws cis with
            | (_, ')') :: cis -> Ok (Some expr, cis)
            | (i, _) :: cis -> Error (Some i)
            | [] -> Error None)
        | Error err -> Error err)
    | [] -> Ok (None, [])
    | cis -> (
        match read_ident true cis with
        | Ok (Some ident, cis) ->
            let expr =
              match StringMap.find_opt ident bvars with
              | Some idx -> BoundVar (depth - idx - 1)
              | None -> FreeVar ident
            in
            Ok (Some expr, cis)
        | Ok (None, cis) -> Ok (None, cis)
        | Error err -> Error err)

  (* parser for <lambda_expr> *)
  and read_lambda_expr depth bvars cis =
    let rec read_params fst params depth bvars cis =
      match read_ws cis with
      | (_, '.') :: cis when not fst -> Ok (params, depth, bvars, cis)
      | cis -> (
          match read_ident false cis with
          | Ok (Some ident, cis) ->
              read_params false
                (fun rst -> params (Abstract (ident, rst)))
                (depth + 1)
                (StringMap.add ident depth bvars)
                cis
          | Ok (None, (i, _) :: _) -> Error (Some i)
          | Ok (None, []) -> Error None
          | Error err -> Error err)
    in
    match cis with
    | (_, '\\') :: cis | (_, '\206') :: (_, '\187') :: cis -> (
        match read_params true Fun.id depth bvars cis with
        | Ok (params, depth, bvars, cis) -> (
            match read_expr depth bvars cis with
            | Ok (expr, cis) -> Ok (params expr, cis)
            | Error err -> Error err)
        | Error err -> Error err)
    | (i, _) :: _ -> Error (Some i)
    | [] -> Error None

  (* parser for <expr> *)
  and read_expr depth bvars cis =
    let rec read_applied_exprs chain cis =
      let cis = read_ws cis in
      match cis with
      | (_, '\\') :: _ | (_, '\206') :: (_, '\187') :: _ -> (
          match read_lambda_expr depth bvars cis with
          | Ok (expr, cis) -> Ok (Apply (chain, expr), cis)
          | Error err -> Error err)
      | cis -> (
          match read_closed_expr depth bvars cis with
          | Ok (Some expr, cis) -> read_applied_exprs (Apply (chain, expr)) cis
          | Ok (None, cis) -> Ok (chain, cis)
          | Error err -> Error err)
    in
    let cis = read_ws cis in
    match cis with
    | (_, '\\') :: _ | (_, '\206') :: (_, '\187') :: _ ->
        read_lambda_expr depth bvars cis
    | cis -> (
        match read_closed_expr depth bvars cis with
        | Ok (Some expr, cis) -> read_applied_exprs expr cis
        | Ok (None, cis) -> (
            match cis with (i, _) :: cis -> Error (Some i) | _ -> Error None)
        | Error err -> Error err)

  (* parser for <stmt> *)
  let read_stmt cis =
    let rec read_params params depth bvars cis =
      match read_ws cis with
      | (_, '=') :: cis -> Ok (params, depth, bvars, cis)
      | cis -> (
          match read_ident false cis with
          | Ok (Some ident, cis) ->
              read_params
                (fun rst -> params (Abstract (ident, rst)))
                (depth + 1)
                (StringMap.add ident depth bvars)
                cis
          | Ok (None, cis) -> (
              match cis with (i, _) :: _ -> Error (Some i) | _ -> Error None)
          | Error err -> Error err)
    in
    match cis |> read_ws |> read_ident false with
    | Ok (Some decl_name, cis) -> (
        match read_params Fun.id 0 StringMap.empty cis with
        | Ok (params, depth, bvars, cis) -> (
            match read_expr depth bvars cis with
            | Ok (expr, cis) -> Ok (decl_name, expr, cis)
            | Error err -> Error err)
        | Error err -> Error err)
    | Ok (None, (i, _) :: _) -> Error (Some i)
    | Ok (None, []) -> Error None
    | Error err -> Error err

  let expr str =
    match
      str |> String.to_seqi |> List.of_seq |> read_expr 0 StringMap.empty
    with
    | Ok (expr, cis) -> (
        match read_ws cis with
        | [] | (_, ';') :: _ -> Ok expr
        | (i, _) :: _ -> Error (Some i))
    | Error err -> Error err

  let stmt str =
    match str |> String.to_seqi |> List.of_seq |> read_stmt with
    | Ok (decl_name, expr, cis) -> (
        match read_ws cis with
        | [] | (_, ';') :: _ -> Ok (decl_name, expr)
        | (i, _) :: _ -> Error (Some i))
    | Error err -> Error err

  let repr_ex fmt expr =
    let string_to_dlist = String.fold_right (fun c rst -> c :: rst) in
    let escape_ident ident =
      if
        match ident.[0] with
        | 'a' .. 'z' | 'A' .. 'Z' ->
            String.for_all
              (function '0' .. '9' -> true | _ -> false)
              (suffix 1 ident)
        | _ -> false
      then ident
      else "{" ^ ident ^ "}"
    in
    let l_paren = fmt (PunctFmt LParen) |> String.to_seq |> List.of_seq in
    let r_paren = fmt (PunctFmt RParen) |> String.to_seq |> List.of_seq in
    let lambda = fmt (PunctFmt Lambda) |> String.to_seq |> List.of_seq in
    let dot = fmt (PunctFmt Dot) |> String.to_seq |> List.of_seq in
    let rec dfs is_in_abs bvars expr =
      match expr with
      | BoundVar idx ->
          let ident =
            match List.nth_opt bvars idx with Some s -> s | None -> "?"
          in
          let fmt =
            fmt (BoundVarFmt (List.length bvars - idx, escape_ident ident))
          in
          (false, string_to_dlist fmt)
      | FreeVar ident ->
          let fmt = fmt (FreeVarFmt (escape_ident ident)) in
          (false, string_to_dlist fmt)
      | Apply (l_expr, r_expr) ->
          let l_is_open, l_repr = dfs false bvars l_expr in
          let l_closed =
            if l_is_open then fun rst -> l_paren @ l_repr (r_paren @ rst)
            else l_repr
          in
          let r_is_open, r_repr = dfs false bvars r_expr in
          let r_is_app = match r_expr with Apply _ -> true | _ -> false in
          let r_closed =
            if r_is_app then fun rst -> l_paren @ r_repr (r_paren @ rst)
            else r_repr
          in
          let repr rst = l_closed (r_closed rst) in
          (r_is_open && not r_is_app, repr)
      | Abstract (ident, expr) ->
          let _, inner_repr = dfs true (ident :: bvars) expr in
          let ident =
            fmt (BoundVarFmt (List.length bvars + 1, escape_ident ident))
          in
          let inner_is_abs =
            match expr with Abstract _ -> true | _ -> false
          in
          let dot_inner =
            if inner_is_abs then inner_repr else fun rst -> dot @ inner_repr rst
          in
          let lambda = if is_in_abs then Fun.id else ( @ ) lambda in
          let repr rst = lambda (string_to_dlist ident (dot_inner rst)) in
          (true, repr)
    in
    let _, repr = dfs false [] expr in
    repr [] |> List.to_seq |> String.of_seq

  let repr =
    repr_ex (function
      | PunctFmt LParen -> "("
      | PunctFmt RParen -> ")"
      | PunctFmt Lambda -> "λ"
      | PunctFmt Dot -> "."
      | BoundVarFmt (_, ident) | FreeVarFmt ident -> ident)
end

module Interp = struct end
