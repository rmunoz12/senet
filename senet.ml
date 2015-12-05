open Lexing

type action = Ast | Semantic | Groupeval | Compile

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
            ("-g", Groupeval) ;
			      ("-c", Compile) ]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in

  try

    let program = Parser.program Scanner.token lexbuf in

      match action with
        Ast -> let listing = Ast.string_of_program program
               in print_string listing
      | Semantic -> let checked_program = Sast.check_program program
               in ignore(checked_program);
                  print_string ("Success!\n")
      | Groupeval -> let checked_program = Sast.check_program program in
                     let checked_program = Cast.group_eval checked_program in
                     print_string (Cast.string_of_program checked_program)
      | Compile -> let checked_program = Sast.check_program program in
                   Compile.translate (Cast.group_eval checked_program)

  with Scanner.LexError(msg,lb) ->
          print_string (string_of_error msg lb); print_newline ()
     | Parser.Error ->
          let msg = "Syntax error: " ^ Lexing.lexeme lexbuf in
          print_string (string_of_error msg lexbuf); print_newline ()
     | Sast.SemError(msg) ->
          let msg = "Semantic error: " ^ msg in
          print_string msg; print_newline ()
