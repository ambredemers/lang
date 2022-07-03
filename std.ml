let std : (string * Sexp_t.sexp_t) list =  [
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
