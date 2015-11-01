(* type action = Ast | Interpret | Bytecode | Compile *)
type action = Ast | Hw

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast) ](* ;
			      ("-i", Interpret);
			      ("-b", Bytecode);
			      ("-c", Compile) ] *)
  else Hw in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
    Ast -> let listing = Ast.string_of_program program
           in print_string listing
  | Hw -> print_string "Hello World"; print_newline ()
  (* | Interpret -> ignore (Interpret.run program)
  | Bytecode -> let listing =
      Bytecode.string_of_prog (Compile.translate program)
    in print_endline listing
  | Compile -> Execute.execute_prog (Compile.translate program) *)

