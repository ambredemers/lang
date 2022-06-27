open Parse

type env_t = (string * sexp_t) list

let rec interpret_sexp (sexp : sexp_t) (env : env_t) : sexp_t =
    match sexp with
    | Atom (Id i) ->
        (match List.assoc_opt i env with
        | Some s -> s
        | None -> Error "interpret error: variable was undefined")
    | Atom (String s) -> Atom (String s)
    | Pair (Atom (Id i), r) ->
        (match i, r with
        | "true", _ | "false", _ | "nil", _ -> Error "interpret error: true, false, and nil are not functions"
        | "quote", r -> r
        | "let", Pair (Atom (Id x), Pair (xe, Pair (be, Atom (Id "nil")))) ->
            (match List.assoc_opt x env with
            | Some s -> Error "interpret error: interpret_sexp let: variable was already defined"
            | None ->
                (match interpret_sexp xe ((x, Error "interpret dummy value") :: env) with
                | Error e -> Error ("interpret error: interpret_sexp cons arg was an error: \"" ^ e ^ "\"")
                | xep -> interpret_sexp be ((x, xep) :: env)))
        | "let", _ -> Error "interpret error: interpret_sexp let was malformed"
        | "cond", Pair (Pair (l, r), rest) ->
            (match interpret_sexp l env with
            | Atom (Id "true") -> interpret_sexp r env
            | Atom (Id "false") -> interpret_sexp (Pair (Atom (Id "cond"), rest)) env
            | _ -> Error " interpret error: interpret_sexp cond invalid arg")
        | "cond", _ -> Error "interpret error: malformed cond"
        | "cons", x ->
            (match interpret_sexp x env with
            | Pair (l, r) -> Pair (interpret_sexp l env, interpret_sexp r env)
            | Atom a -> Error ("interpret error: interpret_sexp cons arg was an atom: \"" ^ Parse.string_of_atom a ^ "\"")
            | Error e -> Error ("interpret error: interpret_sexp cons arg was an error: \"" ^ e ^ "\""))
        | "car", x ->
            (match interpret_sexp x env with
            | Pair (l, r) -> interpret_sexp l env
            | Atom a -> Error ("interpret error: interpret_sexp car arg was an atom: \"" ^ Parse.string_of_atom a ^ "\"")
            | Error e -> Error ("interpret error: interpret_sexp car arg was an error: \"" ^ e ^ "\""))
        | "cdr", x ->
            (match interpret_sexp x env with
            | Pair (l, r) -> interpret_sexp r env
            | Atom a -> Error ("interpret error: interpret_sexp cdr arg was an atom: \"" ^ Parse.string_of_atom a ^ "\"")
            | Error e -> Error ("interpret error: interpret_sexp cdr arg was an error: \"" ^ e ^ "\""))
        | "eq", x ->
            (match interpret_sexp x env with
            | Pair (Atom l, Atom r) -> if l = r then Atom (Id "true") else Atom (Id "false")
            | Pair (l, r) -> Error "interpret error: interpret_sexp eq args were not a pair of atoms"
            | Atom _ -> Error "interpret error: interpret_sexp eq arg was an atom"
            | Error e -> Error ("interpret error: interpret_sexp eq arg was an error: " ^ e ^ ""))
        | "atom", x ->
            (match interpret_sexp x env with
            | Atom _ -> Atom (Id "true")
            | Pair _ -> Atom (Id "false")
            | Error e -> Error ("interpret error: interpret_sexp cdr arg was an error: " ^ e ^ ""))
        | _ -> interpret_sexp (Pair (interpret_sexp (Atom (Id i)) env, r)) env)
    | Pair (l, r) -> Pair (interpret_sexp l env, interpret_sexp r env)
    | Error _ -> Error "interpret error: interpret_sexp Error"

let keywords : env_t =  [
    ("true", Atom (Id "true"));
    ("false", Atom (Id "false"));
    ("nil", Atom (Id "nil"));
    ("quote", Atom (Id "quote"));
    ("let", Error "attempted to use let as a variable");
    ("cons", Atom (Id "cons"));
    ("car", Atom (Id "car"));
    ("cdr", Atom (Id "cdr"));
    ("eq", Atom (Id "eq"));
    ("atom", Atom (Id "atom"));
]

let interpret (sexp : sexp_t) : sexp_t = interpret_sexp sexp keywords
