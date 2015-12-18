(**
  * C Abstract Syntax Tree (CAST)
  *
  * The CAST is a slightly modifed version of the SAST, where inherited
  * group attributes have been copied, in order, into groups.
  *
  * Also, the second argument to Group is filled in with Some(gdcl).
  *)

open Types
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

let find_group g s =
  if List.exists (fun x -> x.gname = s) g then
    List.find (fun x -> x.gname = s) g
  else
    (* This is an internal dummy gdecl with name "" *)
    { gname = ""; extends = None; par_actuals = None;
      attributes = []; methods = [] }

let tag_groups_vdcl g v = match v.vtype with
    Group(s, _) ->
      let gdcl = find_group g s in
      { v with vtype = Group(s, Some(gdcl)) }
  | _ -> v

let rec tag_groups_field g = function
    Var(vd) -> Var(tag_groups_vdcl g vd)
  | Attrib(vd1, vd2) ->
      let vd1 = tag_groups_vdcl g vd1 in
      let vd2 = tag_groups_vdcl g vd2 in
      Attrib(vd1, vd2)
  | Fun(fd) -> Fun(tag_groups_func g fd)
  | Method(vd, fd) ->
      let vd = tag_groups_vdcl g vd in
      let fd = tag_groups_func g fd in
      Method(vd, fd)
  | Grp(gd) -> Grp(gd)
  | This -> This

and tag_groups_listlit g = function
    Elems(el) -> Elems(List.map (tag_groups_expr g) el)
  | List(ll_list) -> List(List.map (tag_groups_listlit g) ll_list)
  | EmptyList -> EmptyList

and tag_groups_expr g e =
  let detail, typ = e in
  let typ = match typ with
      Group(s, _) -> Group(s, Some(find_group g s))
    | _ as x -> x
  in
  let detail =
    match detail with
        ListLiteral(ll) ->
          let ll = tag_groups_listlit g ll in
          ListLiteral(ll)
      | Field(fe) -> Field(tag_groups_field g fe)
      | Binop(e1, op, e2) ->
          let e1 = tag_groups_expr g e1 in
          let e2 = tag_groups_expr g e2 in
          Binop(e1, op, e2)
      | Assign(fe, e) ->
          let fe = tag_groups_field g fe in
          let e = tag_groups_expr g e in
          Assign(fe, e)
      | Call(vd_opt, fd, el) ->
          let vd_opt = match vd_opt with
              Some(vd) -> Some(tag_groups_vdcl g vd)
            | None -> None
          in
          let fd = tag_groups_func g fd in
          let el = List.map (tag_groups_expr g) el in
          Call(vd_opt, fd, el)
      | Element(e1, e2) ->
          let e1 = tag_groups_expr g e1 in
          let e2 = tag_groups_expr g e2 in
          Element(e1, e2)
      | Uminus(e) -> Uminus(tag_groups_expr g e)
      | Not(e) -> Not(tag_groups_expr g e)
      | Remove(fe1, fe2, ll) ->
          let fe1 = tag_groups_field g fe1 in
          let fe2 = tag_groups_field g fe2 in
          let ll = tag_groups_listlit g ll in
          Remove(fe1, fe2, ll)
      | Place(fe1, fe2, ll) ->
          let fe1 = tag_groups_field g fe1 in
          let fe2 = tag_groups_field g fe2 in
          let ll = tag_groups_listlit g ll in
          Place(fe1, fe2, ll)
      | _ as x -> x
  in
  detail, typ

and tag_groups_stmt g = function
    Block(scope, sl) -> Block(scope, List.map (tag_groups_stmt g) sl)
  | Expression(e) -> Expression(tag_groups_expr g e)
  | Return(e) -> Return(tag_groups_expr g e)
  | Break -> Break
  | Continue -> Continue
  | End -> End
  | Pass(fdcl, e) ->
      let fdcl = tag_groups_func g fdcl in
      let e = tag_groups_expr g e in
      Pass(fdcl, e)
  | If(e, s1, s2) ->
      let e = tag_groups_expr g e in
      let s1 = tag_groups_stmt g s1 in
      let s2 = tag_groups_stmt g s2 in
      If(e, s1, s2)
  | For(vd, el, s) ->
      let vd = tag_groups_vdcl g vd in
      let el = List.map (tag_groups_expr g) el in
      let s = tag_groups_stmt g s in
      For(vd, el, s)
  | While(e, s) ->
      let e = tag_groups_expr g e in
      let s = tag_groups_stmt g s in
      While(e, s)

and tag_groups_func g = function
    BasicFunc(f) ->
      let t = match f.ftype with
          Group(s, _) -> Group(s, Some(find_group g s))
        | _ -> f.ftype
      in
      let l = List.map (tag_groups_vdcl g) f.locals in
      let fl = List.map (tag_groups_vdcl g) f.formals in
      let b = List.map (tag_groups_stmt g) f.body in
      BasicFunc({ f with ftype = t; locals = l; formals = fl; body = b })
  | AssertFunc(f) ->
      let l = List.map (tag_groups_vdcl g) f.alocals in
      let fl = List.map (tag_groups_vdcl g) f.aformals in
      let b = List.map (tag_groups_stmt g) f.abody in
      AssertFunc({ f with alocals =l; aformals = fl; abody = b })

and tag_groups_grp g gd =
  let a = List.map (tag_groups_vdcl g) gd.attributes in
  let m = List.map (tag_groups_func g) gd.methods in
  let pa = match gd.par_actuals with
      Some(el) -> Some(List.map (tag_groups_expr g) el)
    | None -> None
  in
  let ex = match gd.extends with
      Some(par) -> Some(tag_groups_grp g par)
    | None -> None
  in
  { gd with attributes = a; methods = m; par_actuals = pa; extends = ex }

let group_eval (program : program) =
  let setup, turns = program in
  let v, f, g = setup in
  let g = order_attrib g in
  let v = List.map (tag_groups_vdcl g) v in
  let f = List.map (tag_groups_func g) f in
  let g = List.map (tag_groups_grp g) g in
  let turns = List.map (tag_groups_func g) turns in
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
  | Group(s, _) -> s

let string_of_field = function
    Var(v) -> v.vname
  | Fun(f) ->
      (match f with
         BasicFunc(x) -> x.fname
       | AssertFunc(x) -> x.aname)
  | Attrib(v1, v2) -> v1.vname ^ "." ^ v2.vname
  | Method(v, f) -> v.vname ^ "." ^
      (match f with
          BasicFunc(x) -> x.fname
          | AssertFunc(x) -> x.aname)
  | Grp(g) -> g.gname
  | This -> "this"

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
  | Call(vopt, f, el) ->
      let par =
        (match vopt with
            None -> ""
          | Some(v) -> v.vname) in
      par ^ "." ^ string_of_field(Fun(f)) ^
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

let string_of_program (program : program) =
  let s, t = program in
  string_of_setup s ^ string_of_turns t


