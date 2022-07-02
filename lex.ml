open Token_t

let is_digit (c : char) : bool =
    Char.compare c '0' >= 0 &&
    Char.compare c '9' <= 0

let is_alpha (c : char) : bool =
    Char.compare c 'a' >= 0 &&
    Char.compare c 'z' <= 0 ||
    Char.compare c 'A' >= 0 &&
    Char.compare c 'Z' <= 0

let rec lex_int (input : string) (start_position : int) (current_position : int) : int =
    if current_position >= String.length input then
        current_position
    else
        match input.[current_position] with
        | c when is_digit c -> lex_int input start_position (current_position + 1)
        | _ -> current_position

let rec lex_id (input : string) (start_position : int) (current_position : int) : int =
    if current_position >= String.length input then
        current_position
    else
        match input.[current_position] with
        | c when is_alpha c || is_digit c || c = '_' ->
            lex_id input start_position (current_position + 1)
        | _ -> current_position

let rec lex_string (input : string) (start_position : int) (current_position : int) : int =
    if current_position >= String.length input then
        current_position
    else
        match input.[current_position] with
        | '\"' -> current_position
        | _ -> lex_string input start_position (current_position + 1)

let rec lex_token (input : string) (position : int) : token_t list =
    if position >= String.length input then
        []
    else
        match input.[position] with
        | '(' -> Lparen :: lex_token input (position + 1)
        | ')' -> Rparen :: lex_token input (position + 1)
        | '.' ->
            (match input.[position + 1] with
            | '\\' -> Id "lambda" :: lex_token input (position + 2)
            | _ -> Dot :: lex_token input (position + 1))
        | '\'' -> Quote :: lex_token input (position + 1)
        | '+' -> Id "add" :: lex_token input (position + 1)
        | '-' when not (is_digit input.[position + 1]) ->
            Id "sub" :: lex_token input (position + 1)
        | '*' -> Id "mult" :: lex_token input (position + 1)
        | '/' -> Id "div" :: lex_token input (position + 1)
        | '%' -> Id "mod" :: lex_token input (position + 1)
        | '<' ->
            (match input.[position + 1] with
            | '=' -> Id "leq" :: lex_token input (position + 2)
            | _ -> Id "lt" :: lex_token input (position + 1))
        | '>' ->
            (match input.[position + 1] with
            | '=' -> Id "geq" :: lex_token input (position + 2)
            | _ -> Id "gt" :: lex_token input (position + 1))
        | ' ' | '\t' | '\n' -> lex_token input (position + 1)
        | c when is_digit c || c = '-' ->
            let positionp = position + 1 in
            let new_position = lex_int input positionp positionp in
            let value = int_of_string (String.sub input position (new_position - position)) in
            Int value :: lex_token input new_position
        | c when is_alpha c || c = '_' ->
            let positionp = position + 1 in
            let new_position = lex_id input positionp positionp in
            Id (String.sub input position (new_position - position)) :: lex_token input new_position
        | '\"' ->
            let positionp = position + 1 in
            let new_position = lex_string input positionp positionp in
            String (String.sub input positionp (new_position - positionp)) :: lex_token input (new_position + 1)
        | c -> [Error ("Lexing error: lex_token \"" ^ Char.escaped c ^ "\"")]

let lex (input : string) : token_t list = lex_token input 0
