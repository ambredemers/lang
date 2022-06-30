open Lex
open Sexp_t

(*
    BNF grammar:
    <s-exp> ::= "\"" <Lexeme> "\"" | <Lexeme> | "\'" <sexp> | "()" | "(" <s-exp> <list>
    <list> ::= ")" | "." <s-exp> ")" | <s-exp> <list>
*)

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
        let right, restpp = parse_list restp in
        Pair (left, right), restpp
    | _ -> Error "parsing error: parse_sexp _", input
and parse_list (input : token_t list) : sexp_t * token_t list =
    match input with
    | Rparen :: rest -> Atom (Id "nil"), rest
    | Dot :: rest ->
        let right, restp = parse_sexp rest in
        (match restp with
        | Rparen :: restpp -> right, restpp
        | _ -> Error "parsing error: parse_list _", input)
    | [] -> Error "parsing error: end of input", input
    | x ->
        let left, rest = parse_sexp x in
        let right, restp = parse_list rest in
        Pair (left, right), restp

let parse (input : token_t list) : sexp_t =
    match parse_sexp input with
    | sexp, [] -> sexp
    | _, rest -> Error ("parsing error: parse rest was not empty: " ^ Lex.string_of_token_list rest ^ "\n")
