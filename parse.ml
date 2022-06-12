open Lex

type atom_t = string

type sexp_t =
    | Atom of atom_t
    | Pair of sexp_t * sexp_t
    | Error of string

let rec parse_sexp (input : token_t list) : sexp_t * token_t list =
    match input with
    | Lexeme l :: rest -> Atom l, rest
    | Lparen :: rest -> parse_pair_left rest
    | _ -> Error "parsing error: parse_sexp _", input
and parse_pair_left (input : token_t list) : sexp_t * token_t list =
    let left, rest = parse_sexp input in
    match rest with
    | Dot :: restp ->
        let right, restpp = parse_pair_right restp in
        Pair (left, right), restpp
    | _ -> Error "parsing error: parse_pair_left _", input
and parse_pair_right (input : token_t list) : sexp_t * token_t list =
    let right, rest = parse_sexp input in
    match rest with
    | Rparen :: restp -> right, restp
    | _ -> Error "parsing error: parse_pair_right _", input

let parse (input : token_t list) : sexp_t =
    match parse_sexp input with
    | sexp, [] -> sexp
    | _ -> Error "parsing error: parse rest was not empty"

let rec string_of_sexp (sexp : sexp_t) : string =
    match sexp with
    | Atom a -> "Atom \"" ^ a ^ "\""
    | Pair (l, r) -> "Pair (" ^ string_of_sexp l ^ ", " ^ string_of_sexp r ^ ")"
    | Error e -> "Error \"" ^ e ^ "\""

let rec check_sexp_has_no_errors (sexp : sexp_t) : bool =
    match sexp with
    | Atom a -> true
    | Pair (l, r) -> check_sexp_has_no_errors l && check_sexp_has_no_errors r
    | Error _ -> false
    