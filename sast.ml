exception SemError of string

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
        | Mod | And | Or

type bool_lit =
    True
  | False

type t =
    Int
  | Bool
  | Str
  | Void
  | List_t of t
  | Group of string

and list_lit =
    Elems of expression list
  | List of list_lit list
  | EmptyList

and var_decl = {
    vname : string;
    vtype : t;
    vinit : expression option
}

and expr_detail =
    IntLiteral of int
  | StrLiteral of string
  | ListLiteral of list_lit
  | BoolLiteral of bool_lit
  | VoidLiteral
  | Field of field_expr
  | Binop of expression * op * expression
  | Assign of field_expr * expression
  | Call of func_decl * expression list
  | Element of expression * expression
  | Uminus of expression
  | Not of expression
  | Noexpr
  | Remove of field_expr * field_expr * list_lit
  | Place of field_expr * field_expr * list_lit

and expression =
    expr_detail * t

and field_expr =
    Id of string
  | FieldCall of field_expr * string

and statement =
    Block of statement list
  | Expression of expression
  | Return of expression
  | Break
  | Continue
  | If of expression * statement * statement
  | For of expression * expression list * statement
  | While of expression * statement

and basic_func_decl = {
    ftype : t;
    fname : string;
    formals : var_decl list;
    locals : var_decl list;
    body : statement list;
  }

and assert_decl = {
    aname : string;
    aformals : var_decl list;
    alocals : var_decl list;
    abody : statement list;
  }

and func_decl =
    BasicFunc of basic_func_decl
  | AssertFunc of assert_decl

type group_decl = {
    gname : string;
    extends : string;
    attributes : var_decl list;
    methods : func_decl list;
  }

type setup = var_decl list * func_decl list * group_decl list

type turns = func_decl list

type program = setup * turns


type symbol_table = {
  parent : symbol_table option;
  mutable variables : var_decl list;
  functions : func_decl list
}

type translation_environment = {
  scope : symbol_table;
  return_type : t;
}

let rec string_of_t = function
    Int -> "int"
  | Bool -> "bool"
  | Str -> "str"
  | Void -> "void"
  | List_t(vt) ->
      "list[" ^ string_of_t vt ^ "]"
  | Group(s) -> s

let rec id_type_to_t = function
    Ast.Int -> Int
  | Ast.Bool -> Bool
  | Ast.Str -> Str
  | Ast.Void -> Void
  | Ast.List(id_typ) -> List_t(id_type_to_t id_typ)
  | Ast.Group(s) -> Group(s)

let rec find_variable (scope : symbol_table) name =
  try
    List.find (fun v -> v.vname = name) scope.variables
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_variable parent name
    | _ -> raise Not_found

let rec field env = function
    Ast.Id(vname) ->
      let vdecl = try
        find_variable env.scope vname
      with Not_found ->
        raise (SemError("Undeclared identifier: " ^ vname))
      in
      let typ = vdecl.vtype in
      Id(vname), typ
  | Ast.FieldCall(fe, vname) ->
      (* This is a stopgap, what we really need to do is
         check that fe's fields include name *)
      ignore(field env fe); field env (Ast.Id(vname))

let rec check_single_elem env = function
    Ast.IntElem(i) -> IntLiteral(i), Int
  | Ast.StrElem(s) -> StrLiteral(s), Str
  | Ast.IdElem(s) ->
      let vdecl = try
        find_variable env.scope s
      with Not_found ->
        raise (SemError("Undeclared identifier: " ^ s))
      in
      let typ = vdecl.vtype in
      Field(Id(s)), typ
  | Ast.BoolElem(b) ->
      (match b with
          Ast.True -> BoolLiteral(True), Bool
        | Ast.False -> BoolLiteral(False), Bool)

let rec check_elems_list env = function
    [] -> []
  | hd :: rest ->
      let exp = check_single_elem env hd in
      exp :: (check_elems_list env rest)

let rec verify_elems_list_type env typ = function
    [] -> typ
  | (a, b) :: rest ->
      if b = typ then
        verify_elems_list_type env typ rest
      else
        raise (SemError ("List elements are not all of same type: " ^
                         string_of_t typ))

let rec verify_list_of_list_type env typ = function
    [] -> typ
  | (Elems(expr_list), b) :: rest ->
      if b = typ then
        verify_list_of_list_type env typ rest
      else
        raise (SemError ("List elements are not all of same type: " ^
                         string_of_t typ))
  | (List(ll), b) :: rest ->
      if b = typ then
        verify_list_of_list_type env typ rest
      else
        raise (SemError ("List elements are not all of same type: " ^
                         string_of_t typ))
  | (EmptyList, b) :: rest ->
      verify_list_of_list_type env typ rest

let rec listlit env = function
    Ast.Elems(elems_list) ->
      let el = check_elems_list env elems_list in
      let e, typ = List.hd el in
      let typ = verify_elems_list_type env typ el in
      Elems(el), List_t(typ)
  | Ast.List(ll_list) ->
      let list_of_list_with_typ = List.map (listlit env) ll_list in
      let first_list, typ = List.hd list_of_list_with_typ in
      let typ = verify_list_of_list_type env typ list_of_list_with_typ in
      let list_of_list = List.map (fun (l, _) -> l) list_of_list_with_typ in
      List(list_of_list), List_t(typ)
  | Ast.EmptyList ->
      EmptyList, List_t(Void)

let rec find_function (scope : symbol_table) name =
  try
    List.find (fun f -> match f with
                   BasicFunc(x) -> x.fname = name
                 | AssertFunc(x) -> x.aname = name) scope.functions
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_function parent name
    | _ -> raise Not_found

let rec check_func_call env = function
    Ast.Id(s) ->
      let fdecl = try
        find_function env.scope s
      with Not_found ->
        raise (SemError("Function not found: " ^ s))
      in
      let typ = (match fdecl with
          BasicFunc(f) -> f.ftype
        | AssertFunc(f) -> Bool)
      in
      fdecl, typ

let rec expr env = function
    Ast.IntLiteral(i) -> IntLiteral(i), Int
  | Ast.StrLiteral(s) -> StrLiteral(s), Str
  | Ast.ListLiteral(ll) ->
      let l, typ = listlit env ll in
      ListLiteral(l), typ
  | Ast.BoolLiteral(b) ->
      (match b with
          Ast.True -> BoolLiteral(True), Bool
        | Ast.False -> BoolLiteral(False), Bool)
  | Ast.VoidLiteral -> VoidLiteral, Void
  | Ast.Field(fe) ->
      let f, typ = field env fe in
      Field(f), typ
(*  | Ast.Binop(e1, o, e2) -> true *)
(*  | Ast.Assign(fd, e) -> true  *)
  | Ast.Call(fd, el) ->
      let fdcl, typ = check_func_call env fd in
      let expr_list = List.map (expr env) el in
      Call(fdcl, expr_list), typ
(*  | Ast.Element(e1, e2) -> true
  | Ast.Uminus(e) -> true
  | Ast.Not(e) -> true
  | Ast.Noexpr -> true
  | Ast.Remove(fd1, fd2, ll) -> true
  | Ast.Place(fd1, fd2, ll) -> true *)

let require_bool e msg = match e with
    _, Bool -> ()
  | _, _ -> raise (SemError msg)

let rec stmt env = function
    Ast.Expr(e) -> Expression(expr env e)
  | Ast.If(e, s1, s2) ->
      let e = expr env e in (* Check the predicate *)
      require_bool e "Predicate of if must be boolean";
      If(e, stmt env s1, stmt env s2)  (* Check then, else *)

let rec check_for_begin(turns_section) = match turns_section with
  | [] -> false
  | Ast.BasicFunc(f) :: rest ->
      if f.Ast.fname = "begin" then
        true
      else
        check_for_begin(rest)
  | Ast.AssertFunc(f) :: rest -> check_for_begin(rest)

let check_init env = function
    Ast.ExprInit(e) -> Some(expr env e)
  | Ast.NoInit -> None


let check_formal env v =
  let name = v.Ast.vname in
  let already_declared =
    List.exists (fun x -> x.vname = name) env.scope.variables
  in
  if already_declared then
    raise (SemError ("Variable name previously declared in scope: " ^ name))
  else
    let decl =
      { vname = name;
        vtype = id_type_to_t v.Ast.vtype;
        vinit = check_init env v.Ast.vinit}
    in
    env.scope.variables <- decl :: env.scope.variables;
    decl

let check_local env v =
  check_formal env v

let check_basic_func env f =
  let scope' =
    { parent = Some(env.scope);
      variables = [];
      functions = [] } in
  let env' =
    { scope = scope';
      return_type = id_type_to_t f.Ast.ftype; } in
  let fl = List.map (fun v -> check_formal env' v) f.Ast.formals in
  let ll = List.map (fun dcl -> check_local env' dcl) f.Ast.locals in
  let sl = List.map (fun s -> stmt env' s) f.Ast.body in
  BasicFunc({ ftype = id_type_to_t f.Ast.ftype;
              fname = f.Ast.fname;
              formals = fl;
              locals = ll;
              body = sl })

let check_function env = function
    Ast.BasicFunc(f) -> check_basic_func env f
  (* | Ast.AssertFunc(f) -> check_assert_func env f *)

let check_setup setup_section =
  (* not currently implemented *)
  [], [], []

let built_in_funcs =
  let dcl =
    { vname = "print_string";
      vtype = Str;
      vinit = None}
  in
  [BasicFunc({ ftype = Void;
               fname = "print";
               formals = [dcl];
               locals = [];
               body = [] })]

let check_turns turns_section =
  if check_for_begin(turns_section) = false then
    raise (SemError "No begin() function in @turns section")
  else
    let symbols =
      { parent = None ;
        variables = [];
        functions = built_in_funcs } in
    let env =
      { scope = symbols;
        return_type = Void; } in
    List.map (check_function env) turns_section

let check_program (program : Ast.program) =
  let setup_section, turns_section = program in
  let setup_section = check_setup setup_section
  and turns_section = check_turns turns_section in
  setup_section, turns_section

