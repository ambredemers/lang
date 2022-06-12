(* repl *)

let () =
    try (
        let input = ref (Stdlib.read_line ()) in 
        while !input <> "q" do
            print_string (Lex.string_of_token_list (Lex.lex !input));
            print_string "\n";
            input := Stdlib.read_line ()
        done
    ) with End_of_file -> ()
