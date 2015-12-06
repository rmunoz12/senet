open Sast

let prefix_name n =
  "snt_" ^ n

let setup_to_c s =
    "#include <stdbool.h>" ^ "\n" ^
    "#include <stdio.h>" ^ "\n" ^
    "#include <stdlib.h>" ^ "\n" ^
       "\n" ^
    "void (*CUR_TURN)();" ^ "\n" ^
    "int PLAYER_ON_MOVE;" ^ "\n" ^
    "" ^ "\n"

let binop_to_c = function
   Add -> "+"
 | Sub -> "-"
 | Mult -> "*"
 | Div -> "/"
 | Equal -> "=="
 | Neq -> "!="
 | Less -> "<"
 | Leq -> "<="
 | Greater -> ">"
 | Geq -> ">="
 | Mod -> "%"
 | And -> "&&"
 | Or -> "||"

let id_type_to_c = function
  |  Int -> "int "
  |  Bool -> "bool "
  |  Str -> "char* "
  |  Void -> "void"
  (* | List_t(ft) ->
  | Group(s) ->  *)

let rec field_to_c = function
    Var(v) -> prefix_name v.vname
  (* | Fun(f) ->
  | Grp(g) -> *)

let rec function_call_to_c = function
    BasicFunc(f) -> f.fname

let rec printf var = match var with
  | [] -> ""
  | [el_string, typ] ->
  let arg =
    (match typ with
         Bool -> "\"%s\", " ^ el_string ^" ? \"true\" : \"false\"";
       | Int -> "\"%d\", " ^ el_string
       | Str -> "\"%s\" ," ^ el_string )
  in
  "printf(" ^ arg ^ ")"
  | car :: cdr -> (printf [car]) ^ ";\n" ^ (printf cdr)

let rec var_decl_to_c v =
    id_type_to_c v.vtype ^
    prefix_name v.vname ^
    (match v.vinit with
        None -> ""
      | Some(e) ->
          let detail, _ = e in
          " = " ^ expression_to_c detail) ^
    ";"

and expression_to_c = function
    IntLiteral(i) -> string_of_int i
  | StrLiteral(s) -> Ast.escaped_string s
  (* | ListLiteral(ll) -> "" *)
  | BoolLiteral(b) ->
        (match b with True -> "true" | False -> "false")
  (* | VoidLiteral -> "" *)
  | Field(fd) -> field_to_c fd
  | Binop(e1, op, e2) ->
      let d1, _ = e1
      and d2, _ = e2 in
      let d1 = expression_to_c d1
      and d2 = expression_to_c d2 in
      d1 ^ " " ^ binop_to_c op ^ " " ^ d2
  (* | Assign(fd, e) -> "" *)
  | Call(fd, el) ->
        let e = List.map (fun (detail, _) -> detail) el in
        let fname = function_call_to_c fd in
        if fname = "print" then
          let res = List.map (fun (detail, typ) -> expression_to_c detail, typ) el in
          printf res
        else
          prefix_name fname ^ "(" ^
          String.concat "," (List.map expression_to_c e) ^ ")"
  (* | Element(e1, e2) -> "" *)
  | Uminus(e) -> let detail, _ = e in "-(" ^ expression_to_c detail ^ ")"
  | Not(e) -> let detail, _ = e in "!(" ^ expression_to_c detail ^ ")"
  | Noexpr -> ""
  (* | Remove(fd1, fd2, ll) -> ""
  | Place(fd1, fd2, ll) -> "" *)

let rec statement_to_c = function
    Block(scope, slist) -> "{}"
  | Expression(e) ->
      let detail, _ = e in
      expression_to_c(detail) ^ ";"
  | Return(e) -> let detail, _ = e in "return " ^ expression_to_c detail ^ ";\n"
  | Break -> "break;"
  | Continue -> "continue;"
  | If(e, s1, s2) -> let e, _  = e in
                     "if (" ^ expression_to_c e ^ " ) {\n" ^ statement_to_c s1 ^ "\n}" ^
                     "else {\n" ^ statement_to_c s2 ^ "}\n"
  | For(vd, elist, s) -> ""
  | End -> "exit(0);"
  | Pass(e,s) -> let detaill, _ = s in
                     "CUR_TURN = &" ^ prefix_name (function_call_to_c e) ^ ";\n" ^
                     "PLAYER_ON_MOVE = " ^ expression_to_c detaill ^ ";\n"
  | While(e, s) -> "while (" ^ statement_to_c s ^ ")"

let statements_to_c stmts =
    String.concat "\n" (List.map statement_to_c stmts)

let basic_func_to_c f =
    (id_type_to_c f.ftype) ^ " " ^ prefix_name f.fname ^ "(" ^
    String.concat ", " (List.map var_decl_to_c f.formals) ^ ") {\n" ^
    String.concat "\n" (List.map var_decl_to_c f.locals) ^ "\n" ^
    (statements_to_c f.body) ^ "\n" ^
    "}\n"

let declare_turns = function
    BasicFunc(f) ->
      (id_type_to_c f.ftype) ^ " " ^
      prefix_name f.fname ^ "(" ^
      String.concat ", " (List.map var_decl_to_c f.formals) ^ ");"
  | AssertFunc(f) -> ""

let rec turns_to_c = function
    [] -> ""
  | hd :: tl ->
    ( match hd with
        BasicFunc(f) -> basic_func_to_c f
      | AssertFunc(f) -> ""
    ) ^ turns_to_c tl

let senet_to_c (s, t) =
    setup_to_c(s) ^
    "// @turns\n" ^
    String.concat "\n" (List.map declare_turns t) ^ "\n\n" ^
    turns_to_c(t) ^
    "int main() {\n
    CUR_TURN = &snt_begin;\n
    PLAYER_ON_MOVE = 0;\n
    while (true) {\n
    CUR_TURN();\n
    }\n
    return 0;\n
    }\n"

let translate (program : Sast.program) =
    let outfile = open_out "output.c" in
    let ctext = senet_to_c program in
    output_string outfile ctext
