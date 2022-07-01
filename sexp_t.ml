type atom_t =
| Int of int
| Id of string
| String of string

type sexp_t =
| Atom of atom_t
| Pair of sexp_t * sexp_t
| Error of string

let string_of_atom (atom : atom_t) : string =
  (match atom with
  | Int i -> "Atom (Int " ^ string_of_int i ^ ")"
  | Id i -> "Atom (Id \"" ^ i ^ "\")"
  | String s -> "Atom (String \"" ^ s ^ "\")")
let rec string_of_sexp (sexp : sexp_t) : string =
  match sexp with
  | Atom a -> string_of_atom a
  | Pair (l, r) -> "Pair (" ^ string_of_sexp l ^ ", " ^ string_of_sexp r ^ ")"
  | Error e -> "Error \"" ^ e ^ "\""

let rec sexp_has_no_errors (sexp : sexp_t) : bool =
  match sexp with
  | Atom a -> true
  | Pair (l, r) -> sexp_has_no_errors l && sexp_has_no_errors r
  | Error _ -> false

let rec pretty_string_of_sexp (sexp : sexp_t) : string =
  match sexp with
  | Atom a ->
      (match a with
      | Int i -> string_of_int i
      | Id "nil" -> "()"
      | Id i -> i
      | String s -> "\"" ^ s ^ "\"")
  | Pair (Atom (Id "quote"), x) -> "\'" ^ pretty_string_of_sexp x
  | Pair (l, Atom (Id "nil")) -> "(" ^ pretty_string_of_sexp l ^ ")"
  | Pair (l, r) -> "(" ^ pretty_string_of_sexp l ^ pretty_string_of_list r
  | Error e -> "Error \"" ^ e ^ "\""
and pretty_string_of_list (sexp : sexp_t) : string =
  match sexp with
  | Atom (Id "nil") -> ")"
  | Pair (l, r) -> " " ^ pretty_string_of_sexp l ^ pretty_string_of_list r
  | _ -> " . " ^ pretty_string_of_sexp sexp ^ ")"
