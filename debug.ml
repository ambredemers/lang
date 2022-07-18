(* debug repl *)

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
                let lex = Lex.lex2 !input in
                print_string (Token_t.string_of_token_list lex);
                print_string "\n";
                print_string (Sexp_t.string_of_sexp (Parse.parse lex));
                print_string "\n";
                input := Stdlib.read_line ()
        done
    ) with End_of_file -> ()
