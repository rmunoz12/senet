open Sast

let setup_to_c s =
    "#include <stdbool.h>" ^ "\n" ^
    "#include <stdio.h>" ^ "\n" ^
    "#include <stdlib.h>" ^ "\n" ^
    "" ^ "\n"

let id_type_to_c ft = match ft with
  |  Int -> "int "
  |  Bool -> "bool "
  |  Str -> "char* "
  |  _ -> "void"

let var_decls_to_c vdlcs =
    ""

let rec field_expr_to_c = function
    Var(vd) -> if vd.vname = "print" then "printf" else vd.vname

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

let rec expression_to_c = function
    IntLiteral(i) -> string_of_int i
  | StrLiteral(s) -> Ast.escaped_string s
  | ListLiteral(ll) -> ""
  | BoolLiteral(b) ->
        (match b with True -> "true" | False -> "false")
  | VoidLiteral -> ""
  | Field(fd) -> ""
  | Binop(e1, o, e2) -> ""
  | Assign(fd, e) -> ""
  | Call(fd, el) ->
        let e = List.map (fun (detail, _) -> detail) el in
        let fname = function_call_to_c fd in
        if fname = "print" then
          let res = List.map (fun (detail, typ) -> expression_to_c detail, typ) el in
          printf res
        else
          fname ^ "(" ^
          String.concat "," (List.map expression_to_c e) ^ ")"
  | Element(e1, e2) -> ""
  | Uminus(e) -> let detail, _ = e in "-(" ^ expression_to_c detail ^ ")"
  | Not(e) -> let detail, _ = e in "!(" ^ expression_to_c detail ^ ")"
  | Noexpr -> ""
  | Remove(fd1, fd2, ll) -> ""
  | Place(fd1, fd2, ll) -> ""

let rec statement_to_c = function
    Block(scope, slist) -> "{}"
  | Expression(e) ->
      let detail, _ = e in
      expression_to_c(detail) ^ ";"
  | Return(e) -> let detail, _ = e in "return " ^ expression_to_c detail ^ ";\n"
  | Break -> "break;"
  | Continue -> "continue;"
  | If(e, s1, s2) -> "if (" ^ statement_to_c s1 ^ " ) {\n" ^ statement_to_c s2 ^ "\n}\n"
  | For(vd, elist, s) -> ""
  | End -> "exit(0);"
  | Pass(e,s) -> let detaill, _ = s in
                     "CUR_TURN = &" ^ field_expr_to_c e  ^ ";\n"
                     ^ "PLAYER_ON_MOVE = " ^ expression_to_c detaill ^ ";\n"
  | While(e, s) -> ""

let statements_to_c stmts =
    String.concat "\n" (List.map statement_to_c stmts)

let basic_func_to_c f =
    (id_type_to_c f.ftype) ^ " " ^ f.fname ^ "(" ^
    (var_decls_to_c f.formals) ^ ") {\n" ^
    (var_decls_to_c f.locals) ^ "\n" ^
    (statements_to_c f.body) ^ "\n" ^
    "}\n"

let turns_to_c t =
    "// @turns\n" ^

    (match t with
      [] -> ""
    | hd :: tl ->
      ( match hd with
          BasicFunc(f) -> basic_func_to_c f
        | AssertFunc(f) -> ""
      )
    )

let senet_to_c (s, t) =
    setup_to_c(s) ^ turns_to_c(t) ^
    "int main() {\n
    void (* CUR_TURN)() = &begin;\n
    int PLAYER_ON_MOVE = 0;\n
    while (true) {\n
    CUR_TURN();\n
    }\n
    return 0;\n
    }\n"

let translate (program : Sast.program) =
    let outfile = open_out "output.c" in
    let ctext = senet_to_c program in
    output_string outfile ctext (* ;
    print_string ctext *)
