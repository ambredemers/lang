open Parse

type env_t = (string * sexp_t) list

let rec interpret_sexp (sexp : sexp_t) (env : env_t) : sexp_t =
    match sexp with
    | Atom a ->
        (match List.assoc_opt a env with
        | Some s -> s
        | None -> Atom a)
    | Pair (Atom "let", Pair (Atom x, Pair (xe, Pair (be, Atom "nil")))) ->
        (match List.assoc_opt x env with
        | Some s -> Error "interpret error: interpret_sexp let: variable was already defined"
        | None ->
            (match interpret_sexp xe ((x, Error "interpret dummy value") :: env) with
            | Error e -> Error ("interpret error: interpret_sexp cons arg was an error: \"" ^ e ^ "\"")
            | xep -> interpret_sexp be ((x, xep) :: env)))
    | Pair (Atom "let", _) -> Error "interpret error: interpret_sexp let was malformed"
    | Pair (Atom "cond", Pair (Pair (l, r), rest)) ->
        (match interpret_sexp l env with
        | Atom "true" -> interpret_sexp r env
        | Atom "false" -> interpret_sexp (Pair (Atom "cond", rest)) env
        | _ -> Error " interpret error: interpret_sexp cond invalid arg")
    | Pair (Atom "cons", x) ->
        (match interpret_sexp x env with
        | Pair (l, r) -> Pair (interpret_sexp l env, interpret_sexp r env)
        | Atom a -> Error ("interpret error: interpret_sexp cons arg was an atom: \"" ^ a ^ "\"")
        | Error e -> Error ("interpret error: interpret_sexp cons arg was an error: \"" ^ e ^ "\""))
    | Pair (Atom "car", x) ->
        (match interpret_sexp x env with
        | Pair (l, r) -> interpret_sexp l env
        | Atom a -> Error ("interpret error: interpret_sexp car arg was an atom: \"" ^ a ^ "\"")
        | Error e -> Error ("interpret error: interpret_sexp car arg was an error: \"" ^ e ^ "\""))
    | Pair (Atom "cdr", x) ->
        (match interpret_sexp x env with
        | Pair (l, r) -> interpret_sexp r env
        | Atom a -> Error ("interpret error: interpret_sexp cdr arg was an atom: \"" ^ a ^ "\"")
        | Error e -> Error ("interpret error: interpret_sexp cdr arg was an error: \"" ^ e ^ "\""))
    | Pair (Atom "eq", x) ->
        (match interpret_sexp x env with
        | Pair (Atom l, Atom r) -> if l = r then Atom "T" else Atom "F"
        | Pair (l, r) -> Error "interpret error: interpret_sexp eq args were not a pair of atoms"
        | Atom _ -> Error "interpret error: interpret_sexp eq arg was an atom"
        | Error e -> Error ("interpret error: interpret_sexp eq arg was an error: " ^ e ^ ""))
    | Pair (Atom "atom", x) ->
        (match interpret_sexp x env with
        | Atom _ -> Atom "true"
        | Pair _ -> Atom "false"
        | Error e -> Error ("interpret error: interpret_sexp cdr arg was an error: " ^ e ^ ""))
    | Pair (l, r) -> Pair (interpret_sexp l env, interpret_sexp r env)
    | Error _ -> Error "interpret error: interpret_sexp Error"

let keywords : env_t =  [
    ("let", Error "attempted to use let as a variable");
    ("true", Atom "true");
    ("false", Atom "false");
    ("nil", Atom "nil");
    ("cons", Atom "cons");
    ("car", Atom "car");
    ("cdr", Atom "cdr");
    ("eq", Atom "eq");
    ("atom", Atom "atom");
]

let interpret (sexp : sexp_t) : sexp_t = interpret_sexp sexp keywords
