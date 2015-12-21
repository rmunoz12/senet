open Types
open Sast

type counter = {
  mutable i : int
}

let count = { i = 0 }

let tmp_formal_prefix = "__tmp_form__"

let prefix_name n =
  "snt_" ^ n

let senet_header =
  "#include <stdbool.h>" ^ "\n" ^
  "#include <stdio.h>" ^ "\n" ^
  "#include <stdlib.h>" ^ "\n" ^
  "#include <string.h>\n" ^
  "#include \"temp/sen_linked_list.h\"\n" ^
  "#include \"temp/sen_print_base_grps.h\"\n" ^
  "#include \"temp/sen_init_base_grps.h\"\n" ^
  "#include \"temp/sen_read.h\"\n" ^
     "\n" ^
  "struct SENET_NONE {\n" ^
  "  } SENET_NONE;\n" ^
     "\n" ^
  "struct Sen_list snt_SEN_EMPTY_LIST;\n" ^
    "\n" ^
  "char *SENET_STR_CONCAT(char* s1, char* s2) {\n" ^
  "  char *temp = (char * ) malloc(strlen(s1)+ strlen(s2) +1);\n" ^
  "  strcpy(temp, s1);\n" ^
  "  strcat(temp, s2);\n" ^
  "  return temp;\n" ^
  "}\n" ^
    "\n" ^
  "void (*CUR_TURN)();" ^ "\n" ^
  "int snt_PLAYER_ON_MOVE = 0;" ^ "\n" ^
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
    Int -> "int "
  | Bool -> "bool "
  | Str -> "char* "
  | Void -> "void "
  | List_t(typ) -> "struct Sen_list "
  | Group(s, _) -> "struct " ^ prefix_name s ^ " "

let rec field_to_c = function
    Var(v) ->
      if v.vloop then
        "*(" ^ prefix_name v.vname ^ " + __cnt__" ^ prefix_name v.vname ^ ")"
      else
        prefix_name v.vname
  | Fun(f) -> prefix_name
      (match f with
         BasicFunc(x) -> x.fname
       | AssertFunc(x) -> x.aname)
  | Grp(g) -> prefix_name g.gname
  | Attrib(par, child) ->
    let par_name, deref_op =
      if par.vname = "this" then
        "(*this)", "."
      else if par.vloop then
        "(" ^ prefix_name par.vname ^ " + __cnt__" ^ prefix_name par.vname ^ ")", "->"
      else
        prefix_name par.vname, "."
    in
    par_name ^ deref_op ^ prefix_name child.vname
  | Method(v, f) -> raise (SemError ("Internal error: Method matched in field_to_c(), use Call instead"))
  | This -> "(*this)"

let rec function_call_to_c = function
    BasicFunc(f) -> f.fname
  | AssertFunc(f) -> f.aname

let function_group_method = function
    BasicFunc(f) -> f.group_method
  | AssertFunc(f) -> f.a_group_method

let is_built_in_func = function
    BasicFunc(f) -> f.f_is_built_in
  | AssertFunc(f) -> f.a_is_built_in

let rec printf (detail, typ) =
  let e_c_string = expression_to_c detail in
  match typ with
     Bool -> "printf(\"%s\", " ^ e_c_string ^ " ? \"true\" : \"false\""  ^ ")"
   | Int -> "printf(\"%d\", " ^ e_c_string  ^ ")"
   | Str -> "printf(\"%s\", " ^ e_c_string  ^ ")"
   | Group(x, _) ->
      "printf(\"%s\", " ^ prefix_name x ^ "_" ^ prefix_name "__repr__" ^
      "((struct " ^ prefix_name x  ^ "*) " ^ "&" ^ e_c_string ^ ")" ^ ")"
  | Void -> "printf(" ^"\"None\""  ^ ")"
  | List_t(l_typ) ->
    let tmp_var, list_id = match detail with
        Field(_) -> "", e_c_string
      | Call(vd_opt, fd, _) ->
          let tmp_name =
            count.i <- count.i + 1;
            "__tp__" ^ string_of_int count.i
          in
          id_type_to_c typ ^ tmp_name ^ " = " ^ e_c_string,
          tmp_name
      | _ -> "", prefix_name e_c_string
    in
    match l_typ with
      | Group(x, g) ->
          "printGroupList(&" ^ list_id ^ ", " ^
          prefix_name x ^ "_" ^ prefix_name "__repr__" ^ ")"
      | _ ->
        let func = match l_typ with
            Int -> "printInt"
          | Str -> "printStr"
          | Bool -> "printBool"
          | Void -> "printEmptyList"
          | Group(_, _) ->
              raise (SemError ("Internal error: printList call with Group"))
          | List_t(_) ->
              raise (SemError ("Internal error: printList call with List_t"))
        in
        tmp_var ^ ";\n" ^
        "printList(&" ^ list_id ^ ", " ^ func ^ ")"

and formal_to_c is_built_in_func v = match is_built_in_func, v.vtype with
    false, Group(gname, _) ->
      id_type_to_c v.vtype ^ "* " ^ tmp_formal_prefix ^ prefix_name v.vname
  | _, List_t(typ) ->
      id_type_to_c v.vtype ^ "* " ^ tmp_formal_prefix ^ prefix_name v.vname
  | _, _ ->
      id_type_to_c v.vtype ^ prefix_name v.vname

and function_group = function
    BasicFunc(f) -> f.group_method
  | AssertFunc(f) -> f.a_group_method

and ll_elem_to_c = function
    IntLiteral(_, name)
  | StrLiteral(_, name)
  | BoolLiteral(_, name) -> prefix_name name
  | _ as detail -> expression_to_c detail

and push_ll_to_new_list list_id = function
    Elems(el, _) ->
      let push_elem_to_new_list (detail, typ) =
        "push(&" ^ prefix_name list_id ^
        ", (void *) &(" ^ ll_elem_to_c detail ^ "))"
      in
      String.concat ";\n" (List.map push_elem_to_new_list el)
  | EmptyList -> ""

and push_to_new_list list_id (det, typ) = match det with
    ListLiteral(ll) -> push_ll_to_new_list list_id ll
  | _ -> raise (SemError "Unsupported expression type to push to a new list literal")

and var_decl_to_c v = match v.vtype, v.vinit with
    _, None ->
      id_type_to_c v.vtype ^
      prefix_name v.vname ^ ";"
  | List_t(typ), Some(e) ->
      let detail, _ = e in
      let name = expression_to_c detail in
      id_type_to_c v.vtype ^
      prefix_name v.vname ^
      (if v.vname != "" && String.length v.vname > 5 &&
          String.sub v.vname 0 6 = "__ll__" &&
          String.sub name 0 6 = "__ll__" then
        ";\n" ^
        "new_Sen_list(&" ^ prefix_name name ^
        ", sizeof(" ^ id_type_to_c typ ^ "));\n" ^
        push_to_new_list name e ^ ";"
      else
        " = " ^ prefix_name name ^ ";")
  | _, Some(e) ->
      let detail, typ = e in
      (match typ with
          Group(_, _) ->
            id_type_to_c typ ^ "__tmp__" ^ prefix_name v.vname ^
            " = " ^ expression_to_c detail ^ ";\n" ^
            id_type_to_c v.vtype ^ prefix_name v.vname ^
            " = " ^ "*((" ^ id_type_to_c v.vtype ^ "*) &"
            ^ "__tmp__" ^ prefix_name v.vname ^ ");"
        | _ ->
      id_type_to_c v.vtype ^ prefix_name v.vname ^
      " = " ^ expression_to_c detail ^ ";")

and list_lit_to_c = function
    Elems(el, name) -> name
  | EmptyList -> "SEN_EMPTY_LIST"

and actual_to_c (detail, typ) =
  let e_c_string = expression_to_c detail in
  match typ with
      List_t(l_typ) ->
        (match detail with
            Field(_) -> "&" ^ e_c_string
          | _ -> "&" ^ prefix_name e_c_string)
    | Group(name, gdcl) -> "&" ^ e_c_string
    | _ ->  e_c_string

and expression_to_c = function
    IntLiteral(i, name) -> string_of_int i
  | StrLiteral(s, name) -> Ast.escaped_string s
  | ListLiteral(ll) -> list_lit_to_c ll
  | BoolLiteral(b, name) ->
        (match b with True -> "true" | False -> "false")
  | VoidLiteral -> "SENET_NONE"
  | Field(fd) -> field_to_c fd
  | Binop(e1, op, e2) ->
      let d1, t1 = e1 and d2, t2 = e2 in
      let d1 = expression_to_c d1 and d2 = expression_to_c d2 in
      (match t1, t2 with
          Int, Int
        | Bool, Bool ->
            "(" ^ d1 ^ " " ^ binop_to_c op ^ " " ^ d2 ^ ")"
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
          expression_to_c (BoolLiteral(ans, ""))
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
      and detail, typ = e in
      fd ^ " = " ^
      (match detail with
          ListLiteral(_) -> prefix_name (expression_to_c detail)
        | _ -> expression_to_c detail)
  | Call(vopt, fd, el) ->
      (* let e = List.map (fun (detail, _) -> detail) el in *)
      let argc = List.length el in
      let fname = function_call_to_c fd in
      let gname = function_group_method fd in
      (* print, read, stoi all take one argument, discard remainder *)
      if fname = "print" && is_built_in_func fd then
        let e = List.hd el in
        printf e
      else if fname = "read" && is_built_in_func fd then
        let detail, _ = List.hd el in
        "_snt_read(" ^ expression_to_c detail ^ ")"
      else if fname = "clear_input" && is_built_in_func fd then
        "_snt_clear_input()"
      else if fname = "stoi" && is_built_in_func fd then
        let detail, _ = List.hd el in
        "atoi(" ^ expression_to_c detail ^ ")"
      else if fname = "exit" && is_built_in_func fd then
        "exit(0)"
      else
      let class_prefix = function_group fd in
      let class_prefix =
        if class_prefix <> "" then
          prefix_name class_prefix ^ "_"
        else
          ""
      in
      let instance_addr = match vopt with
          None -> ""
        | Some(v) ->
          let var_name =
            if v.vloop then
              "(" ^ prefix_name v.vname ^ " + __cnt__" ^ prefix_name v.vname ^ ")"
            else if v.vname = "this" then
              "this"
            else
              "&" ^ prefix_name v.vname
          in
          "(struct " ^ prefix_name gname ^ " *) " ^
          var_name ^
          (if argc > 0 then ", " else "")
       in
      class_prefix ^
      field_to_c (Fun(fd)) ^ "(" ^ instance_addr ^
      String.concat ", " (List.map actual_to_c el) ^ ")"
  | Element(e1, e2) ->
      let d1, typ = e1 in
      let d2, _ = e2 in
      let c_typ = match typ with
          List_t(x) -> x
        | _ as x -> x
      in
      "*( (" ^ id_type_to_c c_typ ^ "*) " ^
      "list_elem(&" ^ expression_to_c d1 ^ ", " ^ expression_to_c d2 ^ ") )"
  | Uminus(e) -> let detail, _ = e in "-(" ^ expression_to_c detail ^ ")"
  | Not(e) -> let detail, _ = e in "!(" ^ expression_to_c detail ^ ")"
  | Noexpr -> ""
  | Remove(e) ->
      let detail, _ = e in
      expression_to_c detail
  | Place(e) ->
      let detail, _ = e in
      expression_to_c detail

let reinitalize_and_push (detail, typ) = match detail with
    ListLiteral(ll) -> (match ll with
        EmptyList -> ""
      | Elems(el, name) ->
          let e = detail, typ in
          "new_Sen_list(&" ^ prefix_name name ^
            ", sizeof(" ^ id_type_to_c typ ^ "));\n" ^
            push_to_new_list name e ^ ";\n")
  | _ -> raise (SemError ("non-list passed to reinitalize_and_push"))

let rec update_ll_expr (detail, typ) = match detail with
    ListLiteral(ll) -> reinitalize_and_push (detail, typ)
  (* | Field(fe) -> *)
  | Binop(e1, op, e2) -> update_ll_expr e1 ^ update_ll_expr e2
  | Assign(fe, e) -> update_ll_expr e
  | Call(vd_opt, fd, el) -> String.concat "" (List.map update_ll_expr el)
  (* | Element(e1, e2) -> *)
  (* | Uminus(e) of expression *)
  | Not(e) -> update_ll_expr e
  | Remove(e) -> update_ll_expr e
  | Place(e) -> update_ll_expr e
  | _ -> ""

let rec statement_to_c = function
    Block(scope, slist) ->
      "  " ^ String.concat "\n  " (List.map statement_to_c slist)
  | Expression(e) ->
      let detail, _ = e in
      update_ll_expr e ^
      expression_to_c(detail) ^ ";"
  | Return(e) ->
      let detail, _ = e in
      update_ll_expr e ^
      "return " ^ expression_to_c detail ^ ";"
  | Break -> "break;"
  | Continue -> "continue;"
  | If(e, s1, s2) ->
      let det, _  = e in
      update_ll_expr e ^
      "if (" ^ expression_to_c det ^ " ) {\n" ^
      statement_to_c s1 ^ "\n} " ^
      "else {\n" ^ statement_to_c s2 ^ "}\n"
  | For(vd, elist, s) ->
      let n = List.length elist in
      let counter = "__cnt__" ^ prefix_name vd.vname in
      String.concat "" (List.map update_ll_expr elist) ^
      "int " ^ counter ^ " = 0;\n" ^
      id_type_to_c vd.vtype ^ prefix_name vd.vname ^ "[] = " ^
      "{" ^ String.concat ", "
          (List.map (fun (det, typ) -> expression_to_c det) elist) ^ "};\n" ^
      "for ( ; " ^ counter ^ " < " ^ string_of_int n ^ "; ++" ^ counter ^ ") {\n" ^
      statement_to_c s ^ "\n" ^
      "}\n"
  | End -> "exit(0);"
  | Pass(e,s) -> let detaill, _ = s in
                     "CUR_TURN = &" ^ prefix_name (function_call_to_c e) ^ ";\n" ^
                     "snt_PLAYER_ON_MOVE = " ^ expression_to_c detaill ^ ";\n" ^
                     "return;\n"
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

let deref_formal v = match v.vtype with
    Group(gname, _) ->
      id_type_to_c v.vtype ^ prefix_name v.vname ^
      " = *(" ^ tmp_formal_prefix ^ prefix_name v.vname ^ ");"
  | List_t(typ) ->
      id_type_to_c v.vtype ^ prefix_name v.vname ^
      " = *(" ^ tmp_formal_prefix ^ prefix_name v.vname ^ ");"
  | _ -> ""

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
    String.concat ", " (List.map (formal_to_c f.f_is_built_in) f.formals) ^ ") {\n" ^
    String.concat "\n" (List.map deref_formal f.formals) ^ "\n" ^
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
    String.concat ", " (List.map (formal_to_c f.a_is_built_in) f.aformals) ^ ") {\n" ^
    String.concat "\n" (List.map deref_formal f.aformals) ^ "\n" ^
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
  String.concat "\n" (List.map group_decl_to_c g) ^ "\n" ^
  String.concat "\n" (List.map (func_decl_to_c "") f)

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
