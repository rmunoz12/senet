type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
        | Mod | And | Or

type id_type =
    Int
  | Bool
  | Str
  | Void
  | List of id_type
  | Group of string

type var_decl = {
    vname : string;
    vtype : id_type;
}

type expr =
    IntLiteral of int
  | StrLiteral of string
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  (* | Field of string * string *)
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
  | Remove of expr * expr * expr (* TODO: change e3 to list<int> *)
  | Place of expr * expr * expr  (* TODO: change e3 to list<int> *)

type func_decl = {
    fname : string;
    formals : string list;
    locals : var_decl list;
    body : stmt list;
  }

type group_decl = {
    gname : string;
    extends : string;
    attributes : var_decl list;
    methods : func_decl list;
  }

type setup = var_decl list * func_decl list * group_decl list

type turns = func_decl list

type program = setup * turns

let rec string_of_vtype = function
    Int -> "int"
  | Bool -> "bool"
  | Str -> "str"
  | Void -> "void"
  | List(vt) ->
      "list[" ^ string_of_vtype vt ^ "]"
  | Group(s) -> s

let rec string_of_vdecl vdecl =
  string_of_vtype vdecl.vtype ^ " " ^ vdecl.vname ^ ";\n"

let rec string_of_expr = function
    IntLiteral(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
        Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | Mod -> "%"
      | And -> "and" | Or -> "Or" ) ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | StrLiteral(s) -> s
  | Uminus(e) -> "-" ^ string_of_expr e
  | Not(e) -> "not" ^ string_of_expr e
  | Element(e1, e2) ->
      string_of_expr e1 ^ " [" ^ string_of_expr e2 ^ "]"

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
            "{\n" ^ String.concat "" (List.map string_of_expr elist) ^ "}\n" ^
          ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break"
  | Continue -> "continue"
  | Place(e1, e2, e3) ->
      string_of_expr e1 ^ ">>" ^ string_of_expr e2 ^
        "[" ^ string_of_expr e3 ^ "]"
  | Remove(e1, e2, e3) ->
      string_of_expr e1 ^ "<<" ^ string_of_expr e2 ^
        "[" ^ string_of_expr e3 ^ "]"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

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
