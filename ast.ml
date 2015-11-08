type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
        | Mod | And | Or

type id_type =
    Int
  | Bool
  | Str
  | Void
  | List of id_type
  | Group of string

type bool_lit =
    True
  | False

type list_elem =
    IntElem of int
  | StrElem of string
  | IdElem of string
  | BoolElem of bool_lit

type list_lit =
    Elems of list_elem list
  | List of list_lit list
  | EmptyList

type field_expr =
    Id of string
  | FieldCall of field_expr * string

type expr =
    IntLiteral of int
  | StrLiteral of string
  | ListLiteral of list_lit
  | BoolLiteral of bool_lit
  | VoidLiteral
  | Field of field_expr
  | Binop of expr * op * expr
  | Assign of field_expr * expr
  | Call of field_expr * expr list
  | Element of expr * expr
  | Uminus of expr
  | Not of expr
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | Break
  | Continue
  | If of expr * stmt * stmt
  | For of expr * expr list * stmt
  | While of expr * stmt
  | Remove of expr * expr * list_lit
  | Place of expr * expr * list_lit

type init =
    IdInit of string
  | IntInit of int
  | StrInit of string
  | BoolInit of bool_lit
  | ListInit of list_lit
  | NoInit

type var_decl = {
    vname : string;
    vtype : id_type;
    vinit : init
}

type basic_func_decl = {
    ftype : id_type;
    fname : string;
    formals : var_decl list;
    locals : var_decl list;
    body : stmt list;
  }

type assert_decl = {
    fname : string;
    formals : var_decl list;
    locals : var_decl list;
    body : stmt list;
  }

type func_decl =
    BasicFunc of basic_func_decl
  | AssertFunc of assert_decl

type group_decl = {
    gname : string;
    extends : string;
    attributes : var_decl list;
    methods : func_decl list;
  }

type setup = var_decl list * func_decl list * group_decl list

type turns = func_decl list

type program = setup * turns

let rec escaped_string s =
  Printf.sprintf "%S" s

let rec string_of_vtype = function
    Int -> "int"
  | Bool -> "bool"
  | Str -> "str"
  | Void -> "void"
  | List(vt) ->
      "list[" ^ string_of_vtype vt ^ "]"
  | Group(s) -> s

let rec string_of_list_elems = function
    IntElem(i) -> string_of_int i
  | StrElem(s) -> escaped_string s
  | BoolElem(b) -> (match b with True -> "True" | False -> "False")
  | IdElem(s) -> s

let rec string_of_list_lit = function
    EmptyList -> "[]"
  | Elems(e) ->
        "[" ^ String.concat ", " (List.map string_of_list_elems e) ^ "]"
  | List(l) ->
        "[" ^ String.concat ", " (List.map string_of_list_lit l) ^ "]"

let rec string_of_vinit = function
    NoInit -> ""
  | IntInit(i) -> " = " ^ string_of_int i
  | StrInit(s) -> " = " ^ escaped_string s
  | BoolInit(b) -> " = " ^ (match b with True -> "True" | False -> "False")
  | ListInit(l) -> " = " ^ string_of_list_lit l
  | IdInit(s) -> " = " ^ s

let rec string_of_vdecl vdecl =
  string_of_vtype vdecl.vtype ^ " " ^ vdecl.vname ^
  string_of_vinit vdecl.vinit ^ ";\n"

let rec string_of_field = function
    Id(s) -> s
  | FieldCall(f,s) -> string_of_field f ^ "." ^ s

let rec string_of_expr = function
    IntLiteral(l) -> string_of_int l
  | Field(f) -> string_of_field f
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
        Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | Mod -> "%"
      | And -> "and" | Or -> "Or" ) ^ " " ^
      string_of_expr e2
  | Assign(f, e) -> string_of_field f ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      string_of_field f ^
      "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | StrLiteral(s) -> escaped_string s
  | Uminus(e) -> "-" ^ string_of_expr e
  | Not(e) -> "not" ^ string_of_expr e
  | Element(e1, e2) ->
      string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | ListLiteral(l) -> string_of_list_lit l
  | BoolLiteral(b) -> (match b with True -> "True" | False -> "False")
  | VoidLiteral -> "None"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e, elist, s) ->
      "for (" ^ string_of_expr e  ^ " in " ^
            "{\n" ^ String.concat ", " (List.map string_of_expr elist) ^ "}\n" ^
          ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break;\n"
  | Continue -> "continue;\n"
  | Place(e1, e2, l) ->
      string_of_expr e1 ^ " >> " ^ string_of_expr e2 ^ " << " ^
        string_of_list_lit l ^ ";\n"
  | Remove(e1, e2, l) ->
      string_of_expr e1 ^ " << " ^ string_of_expr e2 ^ " << " ^
        string_of_list_lit l ^ ";\n"

let string_of_basic_fdecl fdecl =
  "func" ^ " " ^ string_of_vtype fdecl.ftype ^ " " ^
  fdecl.fname ^ "(" ^
        String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_assert_decl fdecl =
  "assert " ^
  fdecl.fname ^ "(" ^
        String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_fdecl = function
    BasicFunc(f) -> string_of_basic_fdecl f
  | AssertFunc(f) -> string_of_assert_decl f

let string_of_gdecl gdecl =
  "group " ^ gdecl.gname ^ "(" ^ gdecl.extends ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl gdecl.attributes) ^
  String.concat "" (List.map string_of_fdecl gdecl.methods) ^
  "};\n"

let string_of_setup (vars, funcs, groups) =
  "@setup {\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^
  String.concat "\n" (List.map string_of_gdecl groups) ^
  "}\n"

let string_of_turns (funcs) =
  "@turns {\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^
  "}\n"

let string_of_program (s, t) =
  string_of_setup s ^ string_of_turns t
