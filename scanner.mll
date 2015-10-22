{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "#"      { comment lexbuf }           (* Comment until EOL *)
| '"'      { str (Buffer.create 20) lexbuf }  (* String until next unescaped quote *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MODULO }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "return" { RETURN }
| "int"    { INT }
| "char"   { CHAR }
| "str"    { STR }
| "bool"   { BOOL }
| "void"   { VOID }
| "list"   { LIST }
| "set"    { SET }
| "class"  { CLASS }
| "new"    { NEW }
| "and"    { AND }
| "or"     { OR }
| "not"    { NOT }
| "assert" { ASSERT }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }

and str buf = parse
  (**
   * String parsing. Modified version of:
   * https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html
   *)
  '"'       { STRING (Buffer.contents buf) }
| '\\' '\\' { Buffer.add_char buf '\\'; str buf lexbuf }
| '\\' 'n'  { Buffer.add_char buf '\n'; str buf lexbuf }
| '\\' '"'  {}
| [^ '"' '\\']+ as lxm
  { Buffer.add_string buf (lxm);
    str buf lexbuf
  }
| _  as char { raise (SyntaxError ("Illegal string character: " ^ Char.escaped char)) }
| eof { raise (SyntaxError ("String is not terminated")) }
