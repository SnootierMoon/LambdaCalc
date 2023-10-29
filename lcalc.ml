module StringMap = Map.Make (String)
module IMap = Map.Make (Int)

let suffix n str = String.sub str n (String.length str - n)

type expr =
  | BoundVar of int
  | FreeVar of string
  | Apply of expr * expr
  | Abstract of string * expr

type fmt_punct = LParen | RParen | Lambda | Dot

type fmt =
  | PunctFmt of fmt_punct
  | BoundVarFmt of int * string
  | FreeVarFmt of string

module type ParseSig = sig
  val expr : string -> (expr, int option) result
  val stmt : string -> (string * expr, int option) result
  val repr_ex : (fmt -> string) -> expr -> string
  val repr : expr -> string
end

module Parse : ParseSig = struct
  (* parser for <ws> *)
  let rec read_ws cis =
    match cis with (_, ' ') :: cis -> read_ws cis | cis -> cis

  (* option-parser for <ident> *)
  let rec read_ident in_expr cis =
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
      | (_, '#') :: cis when in_expr -> read_digits (fun rst -> '#' :: rst) cis
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
          | Ok (Some param_ident, cis) ->
              read_params false
                (fun body -> params (Abstract (param_ident, body)))
                (depth + 1)
                (StringMap.add param_ident depth bvars)
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
          | Ok (Some param_ident, cis) ->
              read_params
                (fun body -> params (Abstract (param_ident, body)))
                (depth + 1)
                (StringMap.add param_ident depth bvars)
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

  let string_to_dlist = String.fold_right (fun c rst -> c :: rst)

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

  let repr_ex fmt expr =
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

module Interp = struct
  type susp_expr =
    | SSuspVar of int
    | SBoundVar of int
    | SFreeVar of string
    | SApply of (susp_expr * susp_expr)
    | SAbstract of (string * susp_expr)

  type susp_val =
    | Susp of susp_expr
    | HeadNf of susp_expr
    | BetaNf of susp_expr

  let rec lift expr =
    match expr with
    | BoundVar idx -> SBoundVar idx
    | FreeVar ident -> SFreeVar ident
    | Apply (l_expr, r_expr) -> SApply (lift l_expr, lift r_expr)
    | Abstract (ident, expr) -> SAbstract (ident, lift expr)

  let rec shift off depth expr =
    if off = 0 then expr
    else
      match expr with
      | SSuspVar key -> SSuspVar key
      | SBoundVar idx when idx >= depth -> SBoundVar (idx + off)
      | SBoundVar idx -> SBoundVar idx
      | SFreeVar ident -> SFreeVar ident
      | SApply (l_expr, r_expr) ->
          SApply (shift off depth l_expr, shift off depth r_expr)
      | SAbstract (ident, expr) -> SAbstract (ident, shift off (depth + 1) expr)

  let rec subst depth key expr =
    match expr with
    | SSuspVar key -> SSuspVar key
    | SBoundVar idx when idx < depth -> SBoundVar idx
    | SBoundVar idx when idx = depth -> SSuspVar key
    | SBoundVar idx -> SBoundVar (idx - 1)
    | SFreeVar ident -> SFreeVar ident
    | SApply (l_expr, r_expr) ->
        SApply (subst depth key l_expr, subst depth key r_expr)
    | SAbstract (ident, expr) -> SAbstract (ident, subst (depth + 1) key expr)

  let rec eta_reduce expr =
    let rec shiftl depth expr =
      match expr with
      | BoundVar idx when idx > depth -> Some (BoundVar (idx - 1))
      | BoundVar idx when idx = depth -> None
      | BoundVar idx -> Some (BoundVar idx)
      | FreeVar ident -> Some (FreeVar ident)
      | Apply (l_expr, r_expr) -> (
          match (shiftl depth l_expr, shiftl depth r_expr) with
          | Some l_expr, Some r_expr -> Some (Apply (l_expr, r_expr))
          | _, _ -> None)
      | Abstract (ident, expr) -> (
          match shiftl (depth + 1) expr with
          | Some expr -> Some (Abstract (ident, expr))
          | None -> None)
    in
    match expr with
    | BoundVar idx -> BoundVar idx
    | FreeVar ident -> FreeVar ident
    | Apply (l_expr, r_expr) -> Apply (eta_reduce l_expr, eta_reduce r_expr)
    | Abstract (ident, Apply (expr, BoundVar 0)) -> (
        match shiftl 0 expr with
        | Some expr -> eta_reduce expr
        | None -> Abstract (ident, Apply (eta_reduce expr, BoundVar 0)))
    | Abstract (ident, expr) -> Abstract (ident, eta_reduce expr)

  let eval expr =
    let rec head_nf depth benv expr =
      match expr with
      | SSuspVar key -> (
          match IMap.find key benv with
          | depth', (HeadNf expr | BetaNf expr) ->
              (benv, shift (depth - depth') 0 expr)
          | depth', Susp expr ->
              let benv, expr = head_nf depth' benv expr in
              ( IMap.add key (depth', HeadNf expr) benv,
                shift (depth - depth') 0 expr ))
      | SBoundVar idx -> (benv, SBoundVar idx)
      | SFreeVar ident -> (benv, SFreeVar ident)
      | SApply (l_expr, r_expr) -> (
          match head_nf depth benv l_expr with
          | benv, SAbstract (ident, expr) ->
              print_endline "performing a beta reduction";
              let n = IMap.cardinal benv in
              head_nf depth
                (IMap.add n (depth, Susp r_expr) benv)
                (subst 0 n expr)
          | benv, l_expr -> (benv, SApply (l_expr, r_expr)))
      | SAbstract (ident, expr) ->
          let benv, expr = head_nf (depth + 1) benv expr in
          (benv, SAbstract (ident, expr))
    and head_to_beta_nf depth benv expr =
      match expr with
      | SSuspVar key -> failwith "eval"
      | SBoundVar idx -> (benv, BoundVar idx)
      | SFreeVar ident -> (benv, FreeVar ident)
      | SApply (l_expr, r_expr) ->
          let benv, l_expr = head_to_beta_nf depth benv l_expr in
          let benv, r_expr = beta_nf depth benv r_expr in
          (benv, Apply (l_expr, r_expr))
      | SAbstract (ident, expr) ->
          let benv, expr = head_to_beta_nf (depth + 1) benv expr in
          (benv, Abstract (ident, expr))
    and beta_nf depth benv expr =
      let benv, expr = head_nf depth benv expr in
      let benv, expr = head_to_beta_nf depth benv expr in
      (benv, expr)
    in
    let _, expr = expr |> lift |> beta_nf 0 IMap.empty in
    eta_reduce expr
end
