type token_t =
    | Lparen
    | Rparen
    | Dot
    | Quote
    | Int of int
    | Id of string
    | String of string
    | Error of string

let string_of_token (token : token_t) : string =
    match token with
    | Lparen -> "Lparen"
    | Rparen -> "Rparen"
    | Dot -> "Dot"
    | Quote -> "Quote"
    | Int i -> "Int \"" ^ string_of_int i ^ "\""
    | Id i -> "Id \"" ^ i ^ "\""
    | String s -> "String \"" ^ s ^ "\""
    | Error e -> "Error \"" ^ e ^ "\""

let string_of_token_list (tokens : token_t list) : string =
    "[" ^ List.fold_left (fun a b -> a ^ b ^ "; ") "" (List.map string_of_token tokens) ^ "]"