open Lex

type atom_t =
    | Id of string
    | String of string

(*
    BNF grammar:
    <s-exp> ::= "\"" <Lexeme> "\"" | <Lexeme> | "\'" <sexp> | "()" | "(" <s-exp> <complex>
    <complex> ::= "." <s-exp> ")" | <list>
    <list> ::= <s-exp> <list> | ")"
*)

type sexp_t =
    | Atom of atom_t
    | Pair of sexp_t * sexp_t
    | Error of string

let rec parse_sexp (input : token_t list) : sexp_t * token_t list =
    match input with
    | Double_quote :: Lexeme l :: Double_quote :: rest -> Atom (String l), rest
    | Single_quote :: rest ->
        let sexp, restp = parse_sexp rest in
        Pair (Atom (Id "quote"), sexp), restp
    | Lexeme l :: rest -> Atom (Id l), rest
    | Lparen :: Rparen :: rest -> Atom (Id "nil"), rest
    | Lparen :: rest ->
        let left, restp = parse_sexp rest in
        let right, restpp = parse_complex restp in
        Pair (left, right), restpp
    | _ -> Error "parsing error: parse_sexp _", input
and parse_complex (input : token_t list) : sexp_t * token_t list =
    match input with
    | Dot :: rest ->
        let right, restp = parse_sexp rest in
        (match restp with
        | Rparen :: restpp -> right, restpp
        | _ -> Error "parsing error: parse_complex _", input)
    | x -> parse_list x
and parse_list (input : token_t list) : sexp_t * token_t list =
    match input with
    | Rparen :: rest -> Atom (Id "nil"), rest
    | x ->
        let left, rest = parse_sexp x in
        let right, restp = parse_list rest in
        Pair (left, right), restp

let parse (input : token_t list) : sexp_t =
    match parse_sexp input with
    | sexp, [] -> sexp
    | _, rest -> Error ("parsing error: parse rest was not empty: " ^ Lex.string_of_token_list rest ^ "\n")

let string_of_atom (atom : atom_t) : string =
    (match atom with
    | Id i -> "Atom (Id \"" ^ i ^ "\")"
    | String s -> "Atom (String \"" ^ s ^ "\")")
let rec string_of_sexp (sexp : sexp_t) : string =
    match sexp with
    | Atom a -> string_of_atom a
    | Pair (l, r) -> "Pair (" ^ string_of_sexp l ^ ", " ^ string_of_sexp r ^ ")"
    | Error e -> "Error \"" ^ e ^ "\""

let rec check_sexp_has_no_errors (sexp : sexp_t) : bool =
    match sexp with
    | Atom a -> true
    | Pair (l, r) -> check_sexp_has_no_errors l && check_sexp_has_no_errors r
    | Error _ -> false

let rec pretty_string_of_sexp (sexp : sexp_t) : string =
    match sexp with
    | Atom a ->
        (match a with
        | Id i -> i
        | String s -> "\"" ^ s ^ "\"")
    | Pair (l, r) -> "(" ^ pretty_string_of_sexp l ^ " . " ^ pretty_string_of_sexp r ^ ")"
    | Error e -> "Error \"" ^ e ^ "\""
