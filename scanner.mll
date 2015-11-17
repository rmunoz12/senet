{ open Parser
  exception LexError of string * Lexing.lexbuf }

rule token = parse
  '\n'            {Lexing.new_line lexbuf; token lexbuf} (* http://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html; http://courses.softlab.ntua.gr/compilers/ocamlyacc-tutorial.pdf *)
| [' ' '\t' '\r'] { token lexbuf } (* Whitespace *)
| "#"      { comment lexbuf }           (* Comment until EOL *)
| '"'      { str (Buffer.create 20) lexbuf }  (* String until next unescaped quote *)
| '.'      { DOT }
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "if"     { IF }
| "else"   { ELSE }
| "elif"   { ELIF }
| "for"    { FOR }
| "while"  { WHILE }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "end"    { END }
| "return" { RETURN }
| "True"   { TRUE }
| "False"  { FALSE }
| "None"   { NONE }
| "in"     { IN }
| "int"    { INT }
| "str"    { STR }
| "bool"   { BOOL }
| "void"   { VOID }
| "list"   { LIST }
| "group"  { GROUP }
| "and"    { AND }
| "or"     { OR }
| "not"    { NOT }
| "assert" { ASSERT }
| "<<"     { REMOVE }
| ">>"     { PLACE }
| "func"   { FUNC }
| "pass"   { PASS }
| "@setup" { SETUP }
| "@turns" { TURNS }
| ['0'-'9']+ as lxm { INTLITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char
    { let msg = "Lexical error: Illegal character: " ^ Char.escaped char in
      raise (LexError(msg, lexbuf)) }

and comment = parse
  '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { comment lexbuf }

and str buf = parse
  (**
   * String parsing. Modified version of:
   * https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
   *)
  '"'       { STRLITERAL (Buffer.contents buf) }
| '\\' '\\' { Buffer.add_char buf '\\'; str buf lexbuf }
| '\\' 'n'  { Buffer.add_char buf '\n'; str buf lexbuf }
| '\\' '"'  { Buffer.add_char buf '\"'; str buf lexbuf }
| [^ '"' '\\']+ as lxm
  { Buffer.add_string buf (lxm);
    str buf lexbuf
  }
| _  as char
    { let msg = "Lexical error: Illegal string character: " ^ Char.escaped char in
      raise (LexError (msg, lexbuf)) }
| eof
    { let msg = "Lexical error: String is not terminated" in
      raise (LexError (msg, lexbuf)) }
