(* debug repl *)

let () =
    try (
        let input = ref (Stdlib.read_line ()) in 
        while !input <> "q" do
            let lex = Lex.lex !input in
            print_string (Token_t.string_of_token_list lex);
            print_string "\n";
            print_string (Sexp_t.string_of_sexp (Parse.parse lex));
            print_string "\n";
            input := Stdlib.read_line ()
        done
    ) with End_of_file -> ()
