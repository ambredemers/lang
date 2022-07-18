(* repl *)

exception Semicolon

let () =
    try (
        let input = ref (Stdlib.read_line ()) in 
        while !input <> "quit" do
            try (while true do
                let temp = String.split_on_char ';' !input in
                if List.length temp <> 1 then
                    (input := List.hd temp;
                    raise Semicolon)
                else if List.hd temp = "quit" then
                    raise End_of_file
                else
                    input := !input ^ "\n" ^ Stdlib.read_line ()    
            done) with Semicolon -> 
                print_string (Sexp_t.pretty_string_of_sexp (Interpret.interpret (Parse.parse (Lex.lex2 !input))));
                print_string "\n";
                input := Stdlib.read_line ()
        done
    ) with End_of_file -> ()
