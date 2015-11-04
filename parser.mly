%{ open Ast
   let fst_of_three (x, _, _) = x
   let snd_of_three (_, x, _) = x
   let trd_of_three (_, _, x) = x  %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA DOT
%token LBRACKET RBRACKET
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD
%token AND OR NOT
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token <int> INTLITERAL
%token <string> STRLITERAL
%token <string> ID
%token EOF
%token IN
%token BREAK CONTINUE
%token ELIF
%token TRUE FALSE NONE
%token STR BOOL VOID LIST GROUP
%token ASSERT
%token REMOVE PLACE
%token SETUP TURNS FUNC

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%nonassoc NOT
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS MOD
%left TIMES DIVIDE
%nonassoc UMINUS
%nonassoc LBRACKET
%left DOT

%start program
%type <Ast.program> program

%%

program:
  twoparts EOF { $1 }

twoparts:
    SETUP LBRACE decls RBRACE TURNS LBRACE fdecl_list RBRACE {$3, $7}

decls:
   /* nothing */ { [], [], [] }
 | decls vdecl { ($2 :: fst_of_three $1),
                 snd_of_three $1,
                 trd_of_three $1 }
 | decls fdecl { fst_of_three $1,
                 ($2 :: snd_of_three $1),
                 trd_of_three $1 }
 | decls gdecl { fst_of_three $1,
                 snd_of_three $1,
                 ($2 :: trd_of_three $1)}

gdecl:
    GROUP ID LPAREN ID RPAREN LBRACE vdecl_list fdecl_list RBRACE SEMI
      { { gname = $2;
          extends = $4;
          attributes = List.rev $7;
          methods = List.rev $8 } }

fdecl_list:
    /* nothing */ { [] }
  | fdecl_list fdecl  { $2 :: $1 }

fdecl:
   FUNC type_id ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { BasicFunc({ ftype = $2;
     fname = $3;
     formals = $5;
     locals = List.rev $8;
     body = List.rev $9 }) }
  | ASSERT ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
      { AssertFunc({ fname = $2;
     formals = $4;
     locals = List.rev $7;
     body = List.rev $8 }) }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    type_id ID                   { [{ vtype = $1; vname = $2; vinit = NoInit }] }
  | formal_list COMMA type_id ID { { vtype = $3; vname = $4; vinit = NoInit } :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    type_id ID SEMI
      { { vtype = $1;
          vname = $2;
          vinit = NoInit } }
  | type_id ID ASSIGN ID SEMI
      { { vtype = $1;
          vname = $2;
          vinit = IdInit($4) } }
  | type_id ID ASSIGN INTLITERAL SEMI
      { { vtype = $1;
          vname = $2;
          vinit = IntInit($4) } }
  | type_id ID ASSIGN STRLITERAL SEMI
      { { vtype = $1;
          vname = $2;
          vinit = StrInit($4) } }
  | type_id ID ASSIGN bool_lit SEMI
      { { vtype = $1;
          vname = $2;
          vinit = BoolInit($4) } }
  | type_id ID ASSIGN list_lit SEMI
      { { vtype = $1;
          vname = $2;
          vinit = ListInit($4) } }

type_id:
    INT                            { Int }
  | BOOL                           { Bool }
  | STR                            { Str }
  | VOID                           { Void }
  | LIST LBRACKET type_id RBRACKET { List($3) }
  | GROUP ID                       { Group($2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt_list_req:
    stmt               { [$1] }
  | stmt_list_req stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN LBRACE stmt_list_req RBRACE %prec NOELSE
     { If($3, Block($6), Block([])) }
  | IF LPAREN expr RPAREN LBRACE stmt_list_req RBRACE ELIF LBRACE stmt_list_req RBRACE
     { If($3, Block($6), Block($10)) }
  | IF LPAREN expr RPAREN LBRACE stmt_list_req RBRACE ELSE LBRACE stmt_list_req RBRACE
     { If($3, Block($6), Block($10)) }
  | FOR LPAREN expr IN LBRACE expr_list RBRACE RPAREN LBRACE stmt_list_req RBRACE
     { For($3, $6, Block($10)) }
  | WHILE LPAREN expr RPAREN LBRACE stmt_list_req RBRACE
     { While($3, Block($6)) }
  | BREAK SEMI    { Break}
  | CONTINUE SEMI { Continue }
  | expr PLACE expr REMOVE list_lit SEMI { Place($1, $3, $5) }
  | expr REMOVE expr REMOVE list_lit SEMI { Remove($1, $3, $5) }

expr:
    INTLITERAL       { IntLiteral($1) }
  | STRLITERAL       { StrLiteral($1) }
  | NONE             { VoidLiteral }
  | bool_lit         { BoolLiteral($1) }
  | list_lit         { ListLiteral($1) }
  | field_expr       { Field($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR    expr  { Binop($1, Or,    $3) }
  | field_expr ASSIGN expr   { Assign($1, $3) }
  | field_expr LPAREN actuals_opt RPAREN { Call($1, $3) }
  | expr LBRACKET expr RBRACKET { Element($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | MINUS expr %prec UMINUS { Uminus($2) }
  | NOT expr { Not($2) }

field_expr:
    ID                     { Id($1) }
  | field_expr DOT ID      { FieldCall($1, $3) }

expr_list:
    expr                 { [$1] }
  | expr_list COMMA expr { $3 :: $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

list_lit:
    LBRACKET RBRACKET                  { EmptyList }
  | LBRACKET list_elems RBRACKET       { Elems($2) }
  | LBRACKET list_of_list_lit RBRACKET { List($2) }

list_of_list_lit:
    list_lit                        { [$1] }
  | list_of_list_lit COMMA list_lit { $3 :: $1 }

list_elems:
    INTLITERAL                  { [IntElem($1)] }
  | STRLITERAL                  { [StrElem($1)] }
  | bool_lit                    { [BoolElem($1)] }
  | ID                          { [IdElem($1)] }
  | list_elems COMMA INTLITERAL { IntElem($3) :: $1 }
  | list_elems COMMA STRLITERAL { StrElem($3) :: $1 }
  | list_elems COMMA bool_lit   { BoolElem($3) :: $1 }
  | list_elems COMMA ID         { IdElem($3) :: $1 }

bool_lit:
    TRUE  { True }
  | FALSE { False }
