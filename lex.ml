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

let rec lex_token (input : string) (position : int) (indent : int) : token_t list =
    if position >= String.length input then
        []
    else
        match input.[position] with
        | '(' -> Lparen :: lex_token input (position + 1) indent
        | ')' -> Rparen :: lex_token input (position + 1) indent
        | '.' -> Dot :: lex_token input (position + 1) indent
        | '=' -> Id "eq" :: lex_token input (position + 1) indent
        | '\'' -> Quote :: lex_token input (position + 1) indent
        | '+' -> Id "add" :: lex_token input (position + 1) indent
        | '-' when not (is_digit input.[position + 1]) ->
            Id "sub" :: lex_token input (position + 1) indent
        | '*' -> Id "mult" :: lex_token input (position + 1) indent
        | '/' -> Id "div" :: lex_token input (position + 1) indent
        | '<' ->
            (match input.[position + 1] with
            | '>' -> Id "neq" :: lex_token input (position + 2) indent
            | '=' -> Id "leq" :: lex_token input (position + 2) indent
            | _ -> Id "lt" :: lex_token input (position + 1) indent)
        | '>' ->
            (match input.[position + 1] with
            | '=' -> Id "geq" :: lex_token input (position + 2) indent
            | _ -> Id "gt" :: lex_token input (position + 1) indent)
        | ' ' | '\t' | '\n' -> lex_token input (position + 1) indent
        | c when is_digit c || c = '-' ->
            let positionp = position + 1 in
            let new_position = lex_int input positionp positionp in
            let value = int_of_string (String.sub input position (new_position - position)) in
            Int value :: lex_token input new_position indent
        | c when is_alpha c || c = '_' ->
            let positionp = position + 1 in
            let new_position = lex_id input positionp positionp in
            Id (String.sub input position (new_position - position)) :: lex_token input new_position indent
        | '\"' ->
            let positionp = position + 1 in
            let new_position = lex_string input positionp positionp in
            String (String.sub input positionp (new_position - positionp)) :: lex_token input (new_position + 1) indent
        | c -> [Error ("Lexing error: lex_token \"" ^ Char.escaped c ^ "\"")]

let lex (input : string) : token_t list = lex_token input 0 0


type lex2_t =
    | Indent of token_t list
    | Dedent of token_t list
    | Tokens of token_t list
    | Error of string

let lex2_of_prelex (p : Prelex.prelex_t) : lex2_t =
    match p with
    | Indent s -> Indent (lex s)
    | Dedent s -> Dedent (lex s)
    | String s -> Tokens (lex s)
    | Error e -> Error e

let starts_w_indent (ls : lex2_t list) : bool =
    match ls with
    | Indent _ :: _ -> true
    | _ -> false

let starts_w_dedent (ls : lex2_t) : bool =
    match ls with
    | Dedent _ -> true
    | _ -> false

let apply_parens (l : lex2_t) (indent : bool) (dedent : bool) : token_t list =
    match l with
    | Indent s | Dedent s | Tokens s ->
        (* start line *)
        (match s with
        | [] -> s
        | Rparen :: [] -> s
        | Rparen :: rest -> Rparen :: Lparen :: rest
        | _ -> Lparen :: s) @
        (* end line *)
        (if indent then
            []
        else if dedent then
            [Rparen; Rparen]
        else
            [Rparen;])
    | Error e -> [Error e;]

let rec imply_parens (l : lex2_t list) : token_t list list =
    match l with
    | [] -> []
    | head :: rest ->
        apply_parens head (starts_w_indent rest) (starts_w_dedent head) ::
        imply_parens rest


let lex2 (input : string) : token_t list =
    List.flatten (imply_parens (List.map lex2_of_prelex (Prelex.prelex input)))