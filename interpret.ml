open Parse

type env_t = (string * sexp_t) list

let rec interpret_sexp (sexp : sexp_t) (env : env_t) : sexp_t =
    match sexp with
    | Atom (Id i) ->
        (match List.assoc_opt i env with
        | Some s -> s
        | None -> Error "interpret error: variable was undefined")
    | Atom (String s) -> Atom (String s)
    | Pair (Atom (Id i), x) ->
        (match i, x with
        | "true", _ | "false", _ | "nil", _ -> Error "interpret error: true, false, and nil are not functions"
        | "quote", r -> r
        | "let", Pair (Atom (Id id), Pair (value, Pair (body, Atom (Id "nil")))) ->
            (match List.assoc_opt id env with
            | Some s -> Error "interpret error: interpret_sexp let: variable was already defined"
            | None ->
                (match interpret_sexp value ((id, Error "interpret dummy value") :: env) with
                | Error e -> Error ("interpret error: interpret_sexp cons arg was an error: \"" ^ e ^ "\"")
                | valuep -> interpret_sexp body ((id, valuep) :: env)))
        | "let", _ -> Error "interpret error: interpret_sexp let was malformed"
        | "cond", Pair (Pair (l, r), rest) ->
            (match interpret_sexp l env with
            | Atom (Id "true") -> interpret_sexp r env
            | Atom (Id "false") -> interpret_sexp (Pair (Atom (Id "cond"), rest)) env
            | _ -> Error " interpret error: interpret_sexp cond invalid arg")
        | "cond", _ -> Error "interpret error: malformed cond"
        | "cons", Pair (l, Pair (r, Atom (Id "nil"))) -> Pair (interpret_sexp l env, interpret_sexp r env)
        | "cons", _ -> Error "interpret error: interpret_sexp cons invalid arg"
        | "car", Pair (a, Atom (Id "nil")) ->
            (match interpret_sexp a env with
            | Pair (l, _) -> interpret_sexp l env
            | _ -> Error "interpret_error: interpret_sexp car arg was not a pair")
        | "car", _ -> Error "interpret error: interpret_sexp car invalid arg count"
        | "cdr", Pair (a, Atom (Id "nil")) ->
            (match interpret_sexp a env with
            | Pair (_, r) -> r
            | _ -> Error "interpret_error: interpret_sexp cdr arg was not a pair")
        | "cdr", _ -> Error "interpret error: interpret_sexp cdr invalid arg count"
        | "eq", Pair (a, Pair (b, Atom (Id "nil"))) ->
            (match interpret_sexp a env, interpret_sexp b env with
            | Atom l, Atom r -> if l = r then Atom (Id "true") else Atom (Id "false")
            | _ -> Error "interpret error: interpret_sexp eq invalid args")
        | "eq", _ -> Error "interpret error: interpret_sexp eq invalid arg count"
        | "atom", Pair (a, Atom (Id "nil")) ->
            (match interpret_sexp a env with
            | Atom _ -> Atom (Id "true")
            | Pair _ -> Atom (Id "false")
            | Error e -> Error ("interpret error: interpret_sexp cdr arg was an error: " ^ e ^ ""))
        | _ -> interpret_sexp (Pair (interpret_sexp (Atom (Id i)) env, x)) env)
    | Pair _ -> Error "interpret error: interpet_sexp Pair"
    | Error _ -> Error "interpret error: interpret_sexp Error"

let keywords : env_t =  [
    ("true", Atom (Id "true"));
    ("false", Atom (Id "false"));
    ("nil", Atom (Id "nil"));
    ("quote", Error "attempted to use quote as a variable");
    ("let", Error "attempted to use let as a variable");
    ("cons", Error "attempted to use cons as a variable");
    ("car", Error "attempted to use car as a variable");
    ("cdr", Error "attempted to use cdr as a variable");
    ("eq", Error "attempted to use eq as a variable");
    ("atom", Error "attempted to use atom as a variable");
]

let interpret (sexp : sexp_t) : sexp_t = interpret_sexp sexp keywords
