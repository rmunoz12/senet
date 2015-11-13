open Lexing

type action = Ast | Semantic | Compile

(**
  * Printing of error line based on:
  * https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
  * http://courses.softlab.ntua.gr/compilers/ocamlyacc-tutorial.pdf
  * http://stackoverflow.com/questions/14046392/verbose-error-with-ocamlyacc?lq=1
  *)

let string_of_error msg lb =
  let pos = lexeme_start_p lb in
  let line = pos.pos_lnum in
  let first_char = lexeme_start lb - pos.pos_bol + 1 in
  let last_char = lexeme_end lb - pos.pos_bol + 1 in

  "line "^ string_of_int line ^ ", " ^
  "characters " ^ string_of_int first_char ^ "-" ^
                  string_of_int last_char ^":\n" ^
  msg


let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast) ;
			      ("-s", Semantic) ;
			      ("-c", Compile) ]
  else Ast in
  let lexbuf = Lexing.from_channel stdin in

  try

    let program = Parser.program Scanner.token lexbuf in


      match action with
        Ast -> let listing = Ast.string_of_program program
               in print_string listing
      | Semantic -> let result = Sast.check_program program
               in print_string (result ^ "\n")
      | Compile -> Compile.translate program

      (* | Compile -> Execute.execute_prog (Compile.translate program) *)

  with Scanner.LexError(msg,lb) ->
          print_string (string_of_error msg lb); print_newline ()
     | Parser.Error ->
          let msg = "Syntax error: " ^ Lexing.lexeme lexbuf in
          print_string (string_of_error msg lexbuf); print_newline ()
