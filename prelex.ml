let tab_width : int = 4

type preprelex_t = int * string option

let rec preprelex_line (input : string) (position : int) (level : int) : preprelex_t =
    if position < String.length input then
        match input.[position] with
        | '\t' -> preprelex_line input (position + 1) (level + 1)
        | ' ' -> preprelex_spaces input (position + 1) 1 level
        | _ -> level, Some (String.sub input position (String.length input - position))
    else
        level, None
and preprelex_spaces (input : string) (position : int) (count : int) (level : int) : preprelex_t =
    if position < String.length input then
        match input.[position] with
        | ' ' when count = tab_width - 1 ->
            preprelex_spaces input (position + 1) 0 (level + 1)
        | ' ' -> preprelex_spaces input (position + 1) (count + 1) level
        | _ when count = 0 -> preprelex_line input position level
        | _ -> level, None
    else
        level, None

let preprelex (input : string) : preprelex_t list =
    let lines = String.split_on_char '\n' input in
    List.map (fun x -> preprelex_line x 0 0) lines

type prelex_t =
    | Indent of string
    | Dedent of string
    | String of string
    | Error of string

let rec prelex_lines (input : preprelex_t list) (last_level : int) : prelex_t list =
    match input with
    | [] -> []
    | (level, None) :: rest ->
        Error "prelex_lines: preprelex error" :: prelex_lines rest level
    | (level, Some str) :: rest ->
        (match level with
        | _ when level = last_level + 1 -> Indent str
        | _ when level = last_level - 1 -> Dedent str
        | _ when level = last_level -> String str
        | _ -> Error "prelex_lines: invalid indentation") :: prelex_lines rest level

let prelex (input : string) : prelex_t list =
    prelex_lines (preprelex input) 0