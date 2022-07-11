let std : (string * Sexp_t.sexp_t) list =
    let f (x : string) : Sexp_t.sexp_t = Parse.parse (Lex.lex x) in
    [
        ("true", Atom (Id "true"));
        ("false", Atom (Id "false"));
        ("nil", Atom (Id "nil"));
        ("let", Error "attempted to use let as a variable");
        ("fn", Error "attempted to use fn as a variable");
        ("cons", Error "attempted to use cons as a variable");
        ("car", Error "attempted to use car as a variable");
        ("cdr", Error "attempted to use cdr as a variable");
        ("eq", Error "attempted to use eq as a variable");
        ("atom", Error "attempted to use atom as a variable");
        ("quote", Error "attempted to use quote as a variable");
        ("cond", Error "attempted to use cond as a variable");
        ("and", f "(fn (andl andr) (cond ((eq andl false) false) ((eq andl true) (cond ((eq andr true) true) ((eq andr false) false)))))");
        ("or", f "(fn (orl orr) (cond ((eq orl true) true) ((eq orl false) (cond ((eq orr true) true) ((eq orr false) false)))))");
        ("not", f "(fn (notx) (cond ((eq notx true) false) ((eq notx false) true)))");
        ("neq", f "(fn (neql neqr) (not (eq neql neqr)))");
    ]
