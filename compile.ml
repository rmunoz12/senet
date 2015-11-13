open Sast
open Ast

let setup_to_c s =
    "@setup{ }" ^ "\n"

let id_type_to_c ft =
    "void"

let var_decls_to_c vdlcs =
    ""

let statements_to_c stmts =
    ""

let basic_func_to_c f =
    (id_type_to_c f.ftype) ^ " " ^ f.fname ^ "(" ^
    (var_decls_to_c f.formals) ^ ") {\n" ^
    (var_decls_to_c f.locals) ^ "\n" ^
    (statements_to_c f.body) ^ "\n" ^
    "}\n"

let turns_to_c t =
    "@turns{\n" ^

    (match t with
      [] -> ""
    | hd :: tl ->
      ( match hd with
          BasicFunc(f) -> basic_func_to_c f
        | AssertFunc(f) -> ""
      )
    ) ^ "}\n"

let senet_to_c(s, t) =
    setup_to_c(s) ^ turns_to_c(t)

let translate program =
    let outfile = open_out "senet.c" in
    let ctext = senet_to_c program in
    output_string outfile ctext;
    print_string ("compiled" ^ "\n")
