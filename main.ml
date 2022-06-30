(* repl *)

let () =
    try (
        let input = ref (Stdlib.read_line ()) in 
        while !input <> "q" do
            print_string (Sexp_t.pretty_string_of_sexp (Interpret.interpret (Parse.parse (Lex.lex !input))));
            print_string "\n";
            input := Stdlib.read_line ()
        done
    ) with End_of_file -> ()
