open Sast
open Ast

let setup_to_c s =
    "" ^ "\n"

let id_type_to_c ft =
    "void"

let var_decls_to_c vdlcs =
    ""

let rec field_expr_to_c = function
    Id(s) -> if s = "print" then "printf" else s
  | FieldCall(fd, s) -> (field_expr_to_c fd) ^ "." ^ s

let rec expression_to_c = function
    IntLiteral(i) -> ""
  | StrLiteral(s) -> Ast.escaped_string s
  | ListLiteral(ll) -> ""
  | BoolLiteral(b) -> ""
  | VoidLiteral -> ""
  | Field(fd) -> ""
  | Binop(e1, o, e2) -> ""
  | Assign(fd, e) -> ""
  | Call(fd, el) -> (field_expr_to_c fd) ^ "(" ^
                    String.concat "," (List.map expression_to_c el) ^ ")"
  | Element(e1, e2) -> ""
  | Uminus(e) -> ""
  | Not(e) -> ""
  | Noexpr -> ""
  | Remove(fd1, fd2, ll) -> ""
  | Place(fd1, fd2, ll) -> ""

let rec statement_to_c = function
    Block(slist) -> "{}"
  | Expr(e) -> expression_to_c(e) ^ ";"
  | Return(e) -> ""
  | Break -> ""
  | Continue -> ""
  | If(e, s1, s2) -> ""
  | For(e, elist, s) -> ""
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

let senet_to_c(s, t) =
    "#include <stdio.h>" ^
    setup_to_c(s) ^ turns_to_c(t) ^
    "int main() {\n
    begin();\n
    return 0;\n
    }\n"

let translate program =
    let outfile = open_out "output.c" in
    let ctext = senet_to_c program in
    output_string outfile ctext;
    print_string ctext
