open Types
open Sast

(* <<<<<<< HEAD

let id_type_to_c ft = match ft with
  |  Int -> "Sen_int *"
  |  Bool -> "Sen_bool *"
  |  Str -> "Sen_str *"
  |  _ -> "void"
======= *)
let prefix_name n =
  "snt_" ^ n

let senet_header =
  "#include \"c_files/headers/all_headers.h\"" ^"\n" ^
     "\n" ^
  "struct SENET_NONE {\n" ^
  "  } SENET_NONE;\n" ^
     "\n" ^
  "char *SENET_STR_CONCAT(char* s1, char* s2) {\n" ^
  "  char *temp = (char * ) malloc(strlen(s1)+ strlen(s2) +1);\n" ^
  "  strcpy(temp, s1);\n" ^
  "  strcat(temp, s2);\n" ^
  "  return temp;\n" ^
  "}\n" ^
    "\n" ^
  "void (*CUR_TURN)();" ^ "\n" ^
  "int snt_PLAYER_ON_MOVE;" ^ "\n" ^
    "\n"

let senet_footer =
  "int main() {\n" ^
  "  CUR_TURN = &snt_begin;\n" ^
  "  snt_PLAYER_ON_MOVE = 0;\n" ^
  "  while (true) {\n" ^
  "    CUR_TURN();\n" ^
  "  }\n" ^
  "  return 0;\n" ^
  "}\n"

let binop_to_c = function
   Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
 | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
 | And -> "&&" | Or -> "||"
 | Equal -> "==" | Neq -> "!="

let id_type_to_c = function
  |  Int -> "int "
  |  Bool -> "bool "
  |  Str -> "char* "
  |  Void -> "void "
  (* | List_t(ft) -> *)
  | Group(s, _) -> "struct " ^ prefix_name s ^ " "
(* >>>>>>> de73297d967f518e7c463fb57cac4913f53340a8 *)

let rec field_to_c = function
    Var(v) -> prefix_name v.vname
  | Fun(f) -> prefix_name
      (match f with
         BasicFunc(x) -> x.fname
       | AssertFunc(x) -> x.aname)
  | Grp(g) -> prefix_name g.gname
  | Attrib(par, child) ->
    let par_name, deref_op =
      if par.vname = "this" then "(*this)", "."
      else prefix_name par.vname, "." in
    par_name ^ deref_op ^ prefix_name child.vname
  | Method(v, f) -> raise (SemError ("Internal error: Method matched in field_to_c(), use Call instead"))
  | This -> "(*this)"

let rec function_call_to_c = function
    BasicFunc(f) -> f.fname
  | AssertFunc(f) -> f.aname

let function_group_method = function
    BasicFunc(f) -> f.group_method
  | AssertFunc(f) -> f.a_group_method

let rec printf var = match var with
  | [] -> ""
  | [el_string, typ] ->
  let arg =
    (match typ with
       Bool -> "\"%s\", " ^ el_string ^ " ? \"true\" : \"false\""
     | Int -> "\"%d\", " ^ el_string
     | Str -> "\"%s\", " ^ el_string
     | Group(x, _) ->
        prefix_name x ^ "_" ^ prefix_name "__repr__" ^
        "((struct " ^ prefix_name x  ^ "*) "  ^ "&" ^ el_string ^ ")"
    | Void -> "\"None\"")
  in
  "printf(" ^ arg ^ ")"
  | car :: cdr -> (printf [car]) ^ ";\n" ^ (printf cdr)

(*
  | [] -> ""
  | [el_string, typ] ->
  let arg =
    | Void -> "printf(\"None\")"
    | _ -> "PRINT(" ^ el_string ^ ")"
  in
  arg ^ ";\n"
  | car :: cdr -> (printf [car]) ^ ";\n" ^ (printf cdr)
*)

(* <<<<<<< HEAD
let rec expression_to_c = function
    IntLiteral(i) -> "CONSTRUCT_INT(" ^ string_of_int i ^ ")"
======= *)
let formal_to_c v =
  id_type_to_c v.vtype ^ prefix_name v.vname

let function_group = function
    BasicFunc(f) -> f.group_method
  | AssertFunc(f) -> f.a_group_method

let rec var_decl_to_c v =
  id_type_to_c v.vtype ^
  prefix_name v.vname ^
  (match v.vinit with
      None -> ""
    | Some(e) ->
        let detail, typ = e in
        " = " ^ expression_to_c detail) ^ ";"

and expression_to_c = function
    IntLiteral(i) -> string_of_int i
(* >>>>>>> de73297d967f518e7c463fb57cac4913f53340a8 *)
  | StrLiteral(s) -> Ast.escaped_string s
  (* | ListLiteral(ll) -> "" *)
  | BoolLiteral(b) ->
        (match b with True -> "true" | False -> "false")
  | VoidLiteral -> "SENET_NONE"
  | Field(fd) -> field_to_c fd
  | Binop(e1, op, e2) ->
      let d1, t1 = e1 and d2, t2 = e2 in
      let d1 = expression_to_c d1 and d2 = expression_to_c d2 in
      (match t1, t2 with
          Int, Int
        | Bool, Bool ->
            d1 ^ " " ^ binop_to_c op ^ " " ^ d2
        | Str, Str ->
          let eval = match op with
              Equal -> "? 0 : 1"
            | Neq -> "? 1 : 0"
            | _ -> ""
          in
          (match op with
             Equal | Neq -> "(strcmp(" ^ d1 ^ ", " ^ d2 ^ ") " ^ eval ^ ")"
           | Add -> "SENET_STR_CONCAT(" ^ d1 ^ ", " ^ d2 ^ ")"
           | _ -> raise (SemError "Binop other than +, ==, or != has lhd and rhs with type Str"))
        | Void, Void ->
          let ans = match op with
              Equal -> True
            | Neq -> False
            | _ ->
              raise (SemError "Binop other than == or != has lhs and rhs with type Void")
          in
          expression_to_c (BoolLiteral(ans))
        | Group(_, gd1), Group(_, gd2) ->
            let gd1, gd2 = match gd1, gd2 with
                Some(x), Some(y) -> x, y
              | _, _ ->
                raise (SemError "Missing a group decl in expression type")
            in
            let eval = match op with
                Equal -> "? 1 : 0"
              | Neq -> "? 0 : 1"
              | _ ->
                raise (SemError "Binop other than == or != has lhs and rhs with type Group(T)")
            in
            let compare_attrib e1 e2 a =
              let e1, _ = e1 and e2, _ = e2 in
              "(" ^ expression_to_c e1 ^ "." ^ prefix_name a.vname ^ " == " ^
              expression_to_c e2 ^ "." ^ prefix_name a.vname ^ ")"
            in
            if gd1.gname == gd2.gname then
              let attrib = gd1.attributes in
              "(" ^ "(" ^
              String.concat " && " (List.map (compare_attrib e1 e2) attrib) ^
              ") " ^ eval ^ ")"
            else
              "false"
        (* | List_t(t1), List_t(t2) -> *)
        | _ , _ ->
          raise (SemError ("Internal error: improper types in binop: " ^
                           d1 ^ " " ^ binop_to_c op ^ " " ^ d2)))
  | Assign(fd, e) ->
      let fd = field_to_c fd
      and detail, _ = e in
      fd ^ " = " ^ expression_to_c detail
  | Call(vopt, fd, el) ->
      let e = List.map (fun (detail, _) -> detail) el in
      let argc = List.length el in
      let fname = function_call_to_c fd in
      let gname = function_group_method fd in
      if fname = "print" then
        let res = List.map (fun (detail, typ) -> expression_to_c detail, typ) el in
        printf res
      else
      let class_prefix = function_group fd in
      let class_prefix =
        if class_prefix <> "" then
          prefix_name class_prefix ^ "_"
        else
          ""
      in
      let instance_addr =
        (match vopt with
            None -> ""
          | Some(v) ->
              "(struct " ^ prefix_name gname ^ " *) " ^
              "&" ^ prefix_name v.vname ^
              (if argc > 0 then ", " else ""))
       in
      class_prefix ^
      field_to_c (Fun(fd)) ^ "(" ^ instance_addr ^
      String.concat ", " (List.map expression_to_c e) ^ ")"
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
                     "snt_PLAYER_ON_MOVE = " ^ expression_to_c detaill ^ ";"
  | While(e, s) ->
      let detail, _ = e in
      "while (" ^ expression_to_c detail ^ ") {\n" ^
      statement_to_c s ^ "\n" ^ "}\n"

let rec assert_stmt_to_c = function
    Block(scope, sl) ->
      "  " ^ String.concat "\n  " (List.map assert_stmt_to_c sl)
  | Expression(e) ->
      let detail, _ = e in
      "if (!(" ^ expression_to_c detail ^ ")) { return false; }\n"
  | If(e, s1, s2) ->
      let e, _  = e in
      "if (" ^ expression_to_c e ^ " ) {\n" ^
      assert_stmt_to_c s1 ^ "\n} " ^
      "else {\n" ^ assert_stmt_to_c s2 ^ "}\n"
  (* | For(vd, el, s) -> *)
  | While(e, s) ->
      let detail, _ = e in
      "while (" ^ expression_to_c detail ^ ") {\n" ^
      assert_stmt_to_c s ^ "\n" ^ "}\n"
  | _ as s -> statement_to_c s

let basic_func_to_c gprefix f =
    let self_arg =
      if f.group_method <> "" then
        "struct " ^ prefix_name f.group_method ^ " *this"
      else
        ""
    in
    let gprefix =
      if gprefix = "" then "" else gprefix ^ "_"
    in
    (id_type_to_c f.ftype) ^ " " ^ gprefix ^ prefix_name f.fname ^ "(" ^
    self_arg ^
    (if self_arg <> "" && List.length f.formals > 0 then ", " else "") ^
    String.concat ", " (List.map formal_to_c f.formals) ^ ") {\n" ^
    String.concat "\n" (List.map var_decl_to_c f.locals) ^ "\n" ^
    String.concat "\n" (List.map statement_to_c f.body) ^ "\n" ^
    "}\n"

let assert_func_to_c gprefix f =
  let self_arg =
      if f.a_group_method <> "" then
        "struct " ^ prefix_name f.a_group_method ^ " *this"
      else
        ""
    in
    let gprefix =
      if gprefix = "" then "" else gprefix ^ "_"
    in
    (id_type_to_c Bool) ^ " " ^ gprefix ^ prefix_name f.aname ^ "(" ^
    self_arg ^
    (if self_arg <> "" && List.length f.aformals > 0 then ", " else "") ^
    String.concat ", " (List.map formal_to_c f.aformals) ^ ") {\n" ^
    String.concat "\n" (List.map var_decl_to_c f.alocals) ^ "\n" ^
    String.concat "\n" (List.map assert_stmt_to_c f.abody) ^ "\n" ^
    "}\n"

let func_decl_to_c gprefix = function
    BasicFunc(f) -> basic_func_to_c gprefix f
  | AssertFunc(f) -> assert_func_to_c gprefix f

let group_decl_to_c g =
  let c_name = prefix_name g.gname in
  "struct " ^ c_name ^  "{\n" ^
  "  " ^ String.concat "\n  " (List.map var_decl_to_c g.attributes) ^ "\n" ^
  "} " ^ c_name ^ ";\n\n" ^
  String.concat "\n" (List.map (func_decl_to_c c_name) g.methods) ^ "\n"

let setup_to_c s =
  let v, f, g = s in
  String.concat "\n" (List.map var_decl_to_c v) ^ "\n\n" ^
  String.concat "\n" (List.map (func_decl_to_c "") f) ^ "\n" ^
  String.concat "\n" (List.map group_decl_to_c g)

let declare_turn = function
    BasicFunc(f) ->
      (id_type_to_c f.ftype) ^ " " ^
      prefix_name f.fname ^ "(" ^
      String.concat ", " (List.map var_decl_to_c f.formals) ^ ");"
  | AssertFunc(f) -> raise (SemError ("Assert function declared in turns: " ^
                                      f.aname))

let turns_to_c t =
  String.concat "\n" (List.map declare_turn t) ^ "\n\n" ^
  String.concat "\n" (List.map (func_decl_to_c "") t)

let senet_to_c (s, t) =
    "// @senet_header\n" ^
    senet_header ^ "\n" ^
    "// @setup\n" ^
    setup_to_c s ^ "\n" ^
    "// @turns\n" ^
    turns_to_c t ^ "\n" ^
    "// @senet_footer\n" ^
    senet_footer

let translate (program : Types.program) =
    let outfile = open_out "output.c" in
    let ctext = senet_to_c program in
    output_string outfile ctext
