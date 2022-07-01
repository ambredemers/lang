open Sexp_t

type env_t = (string * sexp_t) list
let keywords : env_t =  [
    ("true", Atom (Id "true"));
    ("false", Atom (Id "false"));
    ("nil", Atom (Id "nil"));
    ("let", Error "attempted to use let as a variable");
    ("lambda", Error "attempted to use lambda as a variable");
    ("cons", Error "attempted to use cons as a variable");
    ("car", Error "attempted to use car as a variable");
    ("cdr", Error "attempted to use cdr as a variable");
    ("eq", Error "attempted to use eq as a variable");
    ("atom", Error "attempted to use atom as a variable");
    ("quote", Error "attempted to use quote as a variable");
    ("cond", Error "attempted to use cond as a variable");
]

let rec pairlis (x : sexp_t) (y : sexp_t) (env : env_t) : env_t option =
    let rec f (xp : sexp_t) (yp : sexp_t) (envp : env_t) : env_t =
        match xp, yp with
        | Atom (Id "nil"), Atom (Id "nil") -> envp
        | Pair (Atom (Id i), restx), Pair (y, resty) -> (i, y) :: f restx resty envp
        | _ -> raise (Invalid_argument "pairlis _") in
    try Some (f x y env) with Invalid_argument _ -> None

let rec apply (fn : sexp_t) (x : sexp_t) (env : env_t) : sexp_t =
    match fn with
    | Atom (Id i) ->
        (match i, x with
        | "true", _ | "false", _ | "nil", _ -> Error "interpret error: true, false, and nil are not functions"
        | "car", Pair (Pair (l, _), Atom (Id "nil")) -> l
        | "car", h -> Error ("interpret error: apply car invalid arg count" ^ pretty_string_of_sexp h)
        | "cdr", Pair (Pair (_, r), Atom (Id "nil")) -> r
        | "cdr", h -> Error ("interpret error: apply cdr invalid arg count: " ^ pretty_string_of_sexp h)
        | "cons", Pair (l, Pair (r, Atom (Id "nil"))) -> Pair (l, r)
        | "cons", h -> Error ("interpret error: apply cons invalid arg count" ^ pretty_string_of_sexp h)
        | "atom", Pair (a, Atom (Id "nil")) ->
            (match a with
            | Atom _ -> Atom (Id "true")
            | Pair _ -> Atom (Id "false")
            | Error e -> Error ("interpret error: apply cdr arg was an error: " ^ e ^ ""))
        | "atom", h ->
            Error ("interpret error: apply atom invalid arg count: " ^ pretty_string_of_sexp h)
        | "eq", Pair (a, Pair (b, Atom (Id "nil"))) ->
            (match a, b with
            | Atom l, Atom r -> if l = r then Atom (Id "true") else Atom (Id "false")
            | h1, h2 ->
                Error ("interpret error: apply eq invalid arg type: "
                ^ pretty_string_of_sexp h1 ^ ", "
                ^ pretty_string_of_sexp h2))
        | "eq", h -> Error ("interpret error: apply eq invalid arg count: " ^ pretty_string_of_sexp h)
        | _ -> apply (eval (Atom (Id i)) env) x env)
    | Pair (Atom (Id "lambda"), Pair (params, Pair (body, Atom (Id "nil")))) ->
        (match pairlis params x env with
        | Some envp -> eval body envp
        | None -> Error "interpret error: apply lambda envp was None")
    | Pair (Atom (Id "lambda"), h) ->
        Error ("interpret_error: apply lambda invalid arg count: " ^ pretty_string_of_sexp h)
    | h -> Error ("interpret error: apply _: " ^ pretty_string_of_sexp h)
and eval (sexp : sexp_t) (env : env_t) : sexp_t =
    match sexp with
    | Atom (Id i) ->
        (match List.assoc_opt i env with
        | Some (Atom (Id i)) when not (List.mem_assoc i keywords) -> eval (Atom (Id i)) env
        | Some s -> s
        | None -> Error "interpret error: variable was undefined")
    | Atom _ -> sexp
    | Pair (Atom (Id "quote"), x) -> x
    | Pair (Atom (Id "cond"), x) -> evcon x env
    | Pair (Atom (Id "let"), x) -> evlet x env
    | Pair (Atom (Id "lambda"), _) -> sexp
    | Pair (fn, x) -> apply fn (evlis x env) env
    | h -> Error ("interpret_sexp: eval _: " ^ pretty_string_of_sexp h)
and evcon (sexp : sexp_t) (env : env_t) : sexp_t =
    match sexp with
    | Pair (Pair (c, Pair (x, Atom (Id "nil"))), rest) ->
        (match eval c env with
        | Atom (Id "true") -> eval x env
        | Atom (Id "false") -> evcon rest env
        | h -> Error ("interpret error: evcon eval c: " ^ pretty_string_of_sexp h))
    | h -> Error ("interpret error: evcon _: " ^ pretty_string_of_sexp h)
and evlet (sexp : sexp_t) (env : env_t) : sexp_t =
    match sexp with
    | Pair (Atom (Id id), Pair (value, Pair (body, Atom (Id "nil")))) ->
        (match List.assoc_opt id env with
        | Some s -> Error "interpret error: evlet variable was already defined"
        | None -> eval body ((id, eval value env) :: env))
    | h -> Error ("interpret error: evlet _: " ^ pretty_string_of_sexp h)
and evlis (sexp : sexp_t) (env : env_t) : sexp_t =
    match sexp with
    | Atom (Id "nil") -> Atom (Id "nil")
    | Pair (l, r) -> Pair (eval l env, evlis r env)
    | h -> Error ("interpret error: evlis _: " ^ pretty_string_of_sexp h)

let interpret (sexp : sexp_t) : sexp_t = eval sexp keywords
