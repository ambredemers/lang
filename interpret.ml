open Parse

let rec interpret_sexp (sexp : sexp_t) : sexp_t =
    match sexp with
    | Atom a -> Atom a
    | Pair (Atom "cons", x) ->
        (match interpret_sexp x with
        | Pair (l, r) -> Pair (interpret_sexp l, interpret_sexp r)
        | Atom a -> Error ("interpret error: interpret_sexp cons arg was an atom: \"" ^ a ^ "\"")
        | Error e -> Error ("interpret error: interpret_sexp cons arg was an error: \"" ^ e ^ "\""))
    | Pair (Atom "car", x) ->
        (match interpret_sexp x with
        | Pair (l, r) -> interpret_sexp l
        | Atom a -> Error ("interpret error: interpret_sexp car arg was an atom: \"" ^ a ^ "\"")
        | Error e -> Error ("interpret error: interpret_sexp car arg was an error: \"" ^ e ^ "\""))
    | Pair (Atom "cdr", x) ->
        (match interpret_sexp x with
        | Pair (l, r) -> interpret_sexp r
        | Atom a -> Error ("interpret error: interpret_sexp cdr arg was an atom: \"" ^ a ^ "\"")
        | Error e -> Error ("interpret error: interpret_sexp cdr arg was an error: \"" ^ e ^ "\""))
    | Pair (Atom "eq", x) ->
        (match interpret_sexp x with
        | Pair (Atom l, Atom r) -> if l = r then Atom "T" else Atom "F"
        | Pair (l, r) -> Error "interpret error: interpret_sexp eq args were not a pair of atoms"
        | Atom _ -> Error "interpret error: interpret_sexp eq arg was an atom"
        | Error e -> Error ("interpret error: interpret_sexp eq arg was an error: " ^ e ^ ""))
    | Pair (Atom "atom", x) ->
        (match interpret_sexp x with
        | Atom _ -> Atom "T"
        | Pair _ -> Atom "F"
        | Error e -> Error ("interpret error: interpret_sexp cdr arg was an error: " ^ e ^ ""))
    | Pair (l, r) -> Pair (interpret_sexp l, interpret_sexp r)
    | Error _ -> Error "interpret error: interpret_sexp Error"
