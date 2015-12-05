(**
  * C Abstract Syntax Tree (CAST)
  *
  * The CAST is a slightly modifed version of the SAST, where inherited
  * group attributes have been copied, in order, into groups.
  *)

open Sast

let rec get_attributes g = match g.extends with
    None -> g.attributes
  | Some(par) ->
      let pa = get_attributes par in
      pa @ g.attributes

let rec order_attrib = function
    [] -> []
  | g :: rest ->
      let a = get_attributes g in
      let new_g = {g with attributes = a} in
      new_g :: order_attrib rest

let group_eval (program : Sast.program) =
  let setup, turns = program in
  let v, f, g = setup in
  let g = order_attrib g in
  let setup = v, f, g in
  let program = setup, turns in
  program

(**
 * -----------------------------------------
 * Functions to convert the CAST to a string
 * -----------------------------------------
 *)

let rec string_of_t = function
    Int -> "int"
  | Bool -> "bool"
  | Str -> "str"
  | Void -> "void"
  | List_t(vt) ->
      "list[" ^ string_of_t vt ^ "]"
  | Group(s) -> s

let string_of_field = function
    Var(v) -> v.vname
  | Fun(f) ->
      (match f with
         BasicFunc(x) -> x.fname
       | AssertFunc(x) -> x.aname)
  | Grp(g) -> g.gname

let rec string_of_list_lit = function
    EmptyList -> "[]"
  | Elems(e) ->
        "[" ^ String.concat ", " (List.map string_of_expression e) ^ "]"
  | List(l) ->
        "[" ^ String.concat ", " (List.map string_of_list_lit l) ^ "]"

and string_of_expr_detail = function
    IntLiteral(l) -> string_of_int l
  | Field(f) -> string_of_field f
  | Binop(e1, o, e2) ->
      string_of_expression e1 ^ " " ^
      (match o with
        Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | Mod -> "%"
      | And -> "and" | Or -> "or" ) ^ " " ^
      string_of_expression e2
  | Assign(f, e) -> string_of_field f ^ " = " ^ string_of_expression e
  | Call(f, el) ->
      string_of_field(Fun(f)) ^
      "(" ^ String.concat ", " (List.map string_of_expression el) ^ ")"
  | Noexpr -> "<Noexpr>"
  | StrLiteral(s) -> Ast.escaped_string s
  | Uminus(e) -> "-" ^ string_of_expression e
  | Not(e) -> "not" ^ string_of_expression e
  | Element(e1, e2) ->
    string_of_expression e1 ^ "[" ^ string_of_expression e2 ^ "]"
  | ListLiteral(l) -> string_of_list_lit l
  | BoolLiteral(b) -> (match b with True -> "True" | False -> "False")
  | VoidLiteral -> "None"
  | Place(f1, f2, l) ->
      string_of_field f1 ^ " >> " ^ string_of_field f2 ^ " >> " ^
        string_of_list_lit l
  | Remove(f1, f2, l) ->
      string_of_field f1 ^ " << " ^ string_of_field f2 ^ " << " ^
        string_of_list_lit l

and string_of_expression e =
  let detail, _ = e in
  string_of_expr_detail detail

let string_of_vinit = function
    None -> "<None>"
  | Some(e) -> string_of_expression e

let string_of_vdecl vdecl =
  "var_decl = { " ^
  "vtype: " ^ string_of_t vdecl.vtype ^ "; " ^
  "vname: " ^ vdecl.vname ^ "; " ^
  "vinit: " ^ string_of_vinit vdecl.vinit ^
  " }"

let rec string_of_stmt = function
    Block(symbols, stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expression(e) -> string_of_expression e ^ ";\n";
  | Return(e) -> "return " ^ string_of_expression e ^ ";\n";
  | If(e, s1, s2) ->  "if (" ^ string_of_expression e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(vd, elist, s) ->
      "for (" ^ string_of_vdecl vd  ^ " in " ^
            "{\n" ^ String.concat ", " (List.map string_of_expression elist) ^ "}\n" ^
          ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expression e ^ ") {\n" ^ string_of_stmt s ^ "\n}\n"
  | Pass(e, s) -> "pass (" ^ string_of_field(Fun(e)) ^ ", " ^ string_of_expression s ^ ")\n"
  | Break -> "break;\n"
  | Continue -> "continue;\n"
  | End -> "end();\n"

let string_of_basic_fdecl fdecl =
  "basic_func_decl = {\n" ^
  "  ftype: " ^ string_of_t fdecl.ftype ^ "; " ^
  "fname: " ^ fdecl.fname ^ "; " ^
  "turns_func: " ^ string_of_bool fdecl.turns_func ^ ";\n" ^
  "  formals: {\n    " ^
    String.concat ";\n    " (List.map string_of_vdecl fdecl.formals) ^ "\n  }\n" ^
  "  locals: {\n    " ^
    String.concat ";\n    " (List.map string_of_vdecl fdecl.locals) ^ "\n  }\n" ^
  "  body: {\n" ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_assert_decl fdecl =
  "assert_func_decl = {\n" ^
  "  aname: " ^ fdecl.aname ^ "; " ^
  "  a_turns_func: " ^ string_of_bool fdecl.a_turns_func ^ ";\n" ^
  "  aformals: {\n    " ^
    String.concat ";\n    " (List.map string_of_vdecl fdecl.aformals) ^ "\n  }\n" ^
  "  alocals: {\n    " ^
    String.concat ";\n    " (List.map string_of_vdecl fdecl.alocals) ^ "\n  }\n" ^
  "  abody: {\n" ^
    String.concat "" (List.map string_of_stmt fdecl.abody) ^
  "}\n"

let string_of_fdecl = function
    BasicFunc(f) -> string_of_basic_fdecl f
  | AssertFunc(f) -> string_of_assert_decl f

let string_of_gdecl gdecl =
  "group " ^ gdecl.gname ^ "(" ^
      (match gdecl.extends with
           Some(par) -> par.gname ^
              (match gdecl.par_actuals with
                  Some(acts) ->
                    "(" ^ String.concat ", " (List.map string_of_expression acts) ^ ")"
                | None -> "")
         | None -> "") ^ ")\n{\n" ^
  String.concat "" (List.map (fun v -> string_of_vdecl v ^ ";\n") gdecl.attributes) ^
  String.concat "" (List.map string_of_fdecl gdecl.methods) ^
  "};\n"

let string_of_setup s =
  let vars, funcs, groups = s in
  "@setup {\n\n" ^
  String.concat "" (List.map (fun v -> string_of_vdecl v ^ ";\n") vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^
  String.concat "\n" (List.map string_of_gdecl groups) ^
  "\n}\n"

let string_of_turns t =
  "@turns {\n\n" ^
  String.concat "\n" (List.map string_of_fdecl t) ^
  "\n}\n"

let string_of_program (program : Sast.program) =
  let s, t = program in
  string_of_setup s ^ string_of_turns t


