open Sast

let setup_to_c s =
    "" ^ "\n"

let id_type_to_c ft =
    "void"

let var_decls_to_c vdlcs =
    ""

let rec field_expr_to_c = function
    Id(s) -> if s = "print" then "printf" else s
  | FieldCall(fd, s) -> (field_expr_to_c fd) ^ "." ^ s

let rec function_call_to_c = function
    BasicFunc(f) -> f.fname

let printf el_string typ =
  let arg =
    (match typ with
         Bool -> "\"%s\\n\", " ^ el_string ^" ? \"true\" : \"false\"";
       | Int -> "\"%d\", " ^ el_string
       | Str -> "\"%s\" ," ^ el_string )
  in
  "printf(" ^ arg ^ ")"

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
          let _, typ = List.hd el
          and el_string = String.concat "," (List.map expression_to_c e) in
          printf el_string typ
        else
          fname ^ "(" ^
          String.concat "," (List.map expression_to_c e) ^ ")"
  | Element(e1, e2) -> ""
  | Uminus(e) -> ""
  | Not(e) -> ""
  | Noexpr -> ""
  | Remove(fd1, fd2, ll) -> ""
  | Place(fd1, fd2, ll) -> ""

let rec statement_to_c = function
    Block(scope, slist) -> "{}"
  | Expression(e) ->
      let detail, _ = e in
      expression_to_c(detail) ^ ";"
  | Return(e) -> ""
  | Break -> ""
  | Continue -> ""
  | If(e, s1, s2) -> ""
  | For(vd, elist, s) -> ""
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
    "#include <stdio.h>" ^ "\n" ^
    "#include <stdbool.h>" ^ "\n" ^
    setup_to_c(s) ^ turns_to_c(t) ^
    "int main() {\n
    begin();\n
    return 0;\n
    }\n"

let translate (program : Sast.program) =
    let outfile = open_out "output.c" in
    let ctext = senet_to_c program in
    output_string outfile ctext (* ;
    print_string ctext *)
