type token_t =
    | Lparen
    | Rparen
    | Dot
    | Lexeme of string
    | Error of string

let rec lex_string (input : string) (position : int) : token_t list =
    if position >= String.length input then
        []
    else
        match input.[position] with
        | '(' -> Lparen :: lex_string input (position + 1)
        | ')' -> Rparen :: lex_string input (position + 1)
        | '.' -> Dot :: lex_string input (position + 1)
        | ' ' | '\t' | '\n' -> lex_string input (position + 1)
        | _ ->
            let new_position = lex_string_helper input position position in
            Lexeme (String.sub input position (new_position - position)) :: lex_string input new_position
and lex_string_helper (input : string) (start_position : int) (current_position : int) : int =
    if current_position >= String.length input then
        current_position
    else
        match input.[current_position] with
        | '(' | ')' | ' ' | '\t' | '\n' -> current_position
        | _ -> lex_string_helper input start_position (current_position + 1)

let lex (input : string) : token_t list = lex_string input 0

let string_of_token (token : token_t) : string =
    match token with
    | Lparen -> "Lparen"
    | Rparen -> "Rparen"
    | Dot -> "Dot"
    | Lexeme l -> "Lexeme \"" ^ l ^ "\""
    | Error e -> "Error \"" ^ e ^ "\""

let string_of_token_list (tokens : token_t list) : string =
    "[" ^ List.fold_left (fun a b -> a ^ b ^ "; ") "" (List.map string_of_token tokens) ^ "]"
