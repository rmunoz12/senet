open Sast

let prefix_name n =
  "snt_" ^ n

let senet_header =
    "#include <stdbool.h>" ^ "\n" ^
    "#include <stdio.h>" ^ "\n" ^
    "#include <stdlib.h>" ^ "\n" ^
       "\n" ^
    "void (*CUR_TURN)();" ^ "\n" ^
    "int PLAYER_ON_MOVE;" ^ "\n"

let senet_footer =
  "int main() {\n" ^
  "  CUR_TURN = &snt_begin;\n" ^
  "  PLAYER_ON_MOVE = 0;\n" ^
  "  while (true) {\n" ^
  "    CUR_TURN();\n" ^
  "  }\n" ^
  "  return 0;\n" ^
  "}\n"

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
  (* | List_t(ft) -> *)
  | Group(s) -> "struct " ^ s

let rec field_to_c = function
    Var(v) -> prefix_name v.vname
  | Fun(f) -> prefix_name
      (match f with
         BasicFunc(x) -> x.fname
       | AssertFunc(x) -> x.aname)
  | Grp(g) -> prefix_name g.gname

let rec function_call_to_c = function
    BasicFunc(f) -> f.fname

let rec printf var = match var with
  | [] -> ""
  | [el_string, typ] ->
  let arg =
    (match typ with
         Bool -> "\"%s\", " ^ el_string ^ " ? \"true\" : \"false\"";
       | Int -> "\"%d\", " ^ el_string
       | Str -> "\"%s\", " ^ el_string )
  in
  "printf(" ^ arg ^ ")"
  | car :: cdr -> (printf [car]) ^ ";\n" ^ (printf cdr)

let formal_to_c v =
  id_type_to_c v.vtype ^ prefix_name v.vname

let rec var_decl_to_c v =
  id_type_to_c v.vtype ^
  prefix_name v.vname ^
  (match v.vinit with
      None -> ""
    | Some(e) ->
        let detail, _ = e in
        " = " ^ expression_to_c detail) ^ ";"

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
  | Assign(fd, e) ->
      let fd = field_to_c fd
      and detail, _ = e in
      fd ^ " = " ^ expression_to_c detail
  | Call(fd, el) ->
      let e = List.map (fun (detail, _) -> detail) el in
      let fname = function_call_to_c fd in
      if fname = "print" then
        let res = List.map (fun (detail, typ) -> expression_to_c detail, typ) el in
        printf res
      else
        field_to_c (Fun(fd)) ^ "(" ^
        String.concat "," (List.map expression_to_c e) ^ ")"
  (* | Element(e1, e2) -> "" *)
  | Uminus(e) -> let detail, _ = e in "-(" ^ expression_to_c detail ^ ")"
  | Not(e) -> let detail, _ = e in "!(" ^ expression_to_c detail ^ ")"
  | Noexpr -> ""
  (* | Remove(fd1, fd2, ll) -> ""
  | Place(fd1, fd2, ll) -> "" *)

let rec statement_to_c = function
    Block(scope, slist) ->
      "  " ^ String.concat "\n  " (List.map statement_to_c slist)
  | Expression(e) ->
      let detail, _ = e in
      expression_to_c(detail) ^ ";"
  | Return(e) -> let detail, _ = e in "return " ^ expression_to_c detail ^ ";"
  | Break -> "break;"
  | Continue -> "continue;"
  | If(e, s1, s2) ->
      let e, _  = e in
      "if (" ^ expression_to_c e ^ " ) {\n" ^
      statement_to_c s1 ^ "\n} " ^
      "else {\n" ^ statement_to_c s2 ^ "}\n"
  (* | For(vd, elist, s) -> *)
  | End -> "exit(0);"
  | Pass(e,s) -> let detaill, _ = s in
                     "CUR_TURN = &" ^ prefix_name (function_call_to_c e) ^ ";\n" ^
                     "PLAYER_ON_MOVE = " ^ expression_to_c detaill ^ ";"
  | While(e, s) ->
      let detail, _ = e in
      "while (" ^ expression_to_c detail ^ ") {\n" ^
      statement_to_c s ^ "\n" ^ "}\n"

let basic_func_to_c f =
    (id_type_to_c f.ftype) ^ " " ^ prefix_name f.fname ^ "(" ^
    String.concat ", " (List.map formal_to_c f.formals) ^ ") {\n" ^
    String.concat "\n" (List.map var_decl_to_c f.locals) ^ "\n" ^
    String.concat "\n" (List.map statement_to_c f.body) ^ "\n" ^
    "}\n"

let func_decl_to_c = function
    BasicFunc(f) -> basic_func_to_c f
  (* | AssertFunc(f) -> assert_func_to_c a *)

let group_decl_to_c g =
  ""

let setup_to_c s =
  let v, f, g = s in
  String.concat "\n" (List.map var_decl_to_c v) ^
  String.concat "\n" (List.map func_decl_to_c f) ^
  String.concat "\n" (List.map group_decl_to_c g)

let declare_turn = function
    BasicFunc(f) ->
      (id_type_to_c f.ftype) ^ " " ^
      prefix_name f.fname ^ "(" ^
      String.concat ", " (List.map var_decl_to_c f.formals) ^ ");"
  (* | AssertFunc(f) -> "" *)

let turns_to_c t =
  String.concat "\n" (List.map declare_turn t) ^ "\n\n" ^
  String.concat "\n" (List.map func_decl_to_c t)

let senet_to_c (s, t) =
    "// @senet_header\n" ^
    senet_header ^ "\n" ^
    "// @setup\n" ^
    setup_to_c s ^ "\n" ^
    "// @turns\n" ^
    turns_to_c t ^ "\n" ^
    "// @senet_footer\n" ^
    senet_footer

let translate (program : Sast.program) =
    let outfile = open_out "output.c" in
    let ctext = senet_to_c program in
    output_string outfile ctext
