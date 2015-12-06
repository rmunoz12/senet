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
    Var of var_decl
  | Fun of func_decl
  | Grp of group_decl

and statement =
    Block of symbol_table * statement list
  | Expression of expression
  | Return of expression
  | Break
  | Continue
  | End
  | Pass of func_decl * expression
  | If of expression * statement * statement
  | For of var_decl * expression list * statement
  | While of expression * statement

and basic_func_decl = {
    ftype : t;
    fname : string;
    formals : var_decl list;
    locals : var_decl list;
    body : statement list;
    turns_func : bool;
  }

and assert_decl = {
    aname : string;
    aformals : var_decl list;
    alocals : var_decl list;
    abody : statement list;
    a_turns_func : bool
  }

and func_decl =
    BasicFunc of basic_func_decl
  | AssertFunc of assert_decl

and group_decl = {
    gname : string;
    extends : group_decl option;
    par_actuals : expression list option;
    attributes : var_decl list;
    methods : func_decl list;
  }

and setup = var_decl list * func_decl list * group_decl list

and turns = func_decl list

and program = setup * turns


and symbol_table = {
  parent : symbol_table option;
  mutable variables : var_decl list;
  mutable functions : func_decl list;
  mutable groups : group_decl list;
  mutable turns : string list
}

type translation_environment = {
  scope : symbol_table;
  return_type : t;
  in_loop : bool
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

let rec find_function (scope : symbol_table) name =
  try
    List.find (fun f -> match f with
                   BasicFunc(x) -> x.fname = name
                 | AssertFunc(x) -> x.aname = name) scope.functions
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_function parent name
    | _ -> raise Not_found

let rec find_group (scope : symbol_table) name =
  try
    List.find (fun g -> g.gname = name) scope.groups
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_group parent name
    | _ -> raise Not_found

let rec verify_args_signature formals actuals = match formals, actuals with
    [], [] -> true
  | f :: frest, a :: arest ->
    let tf = f.vtype
    and _, ta = a in
    if tf = ta then
      verify_args_signature frest arest
    else
      false
  | _ :: _, [] -> false
  | [], _ :: _ -> false

let rec search_func_in_child (parent : group_decl) actuals name =
  let rec helper = function
      [] -> raise (SemError ("Function name " ^ name ^ " exists in parent methods " ^
                             "but actuals signature not matched"))
    | f :: rest ->
        let formals = (match f with
                            BasicFunc(x) -> x.formals
                          | AssertFunc(x) -> x.aformals)
        in
        if List.length formals = List.length actuals then
          if verify_args_signature formals actuals then
            f
          else
            helper rest
        else
          helper rest
  in
  helper parent.methods

let rec find_child (parent : group_decl) actuals name =
  if List.exists (fun v -> v.vname = name) parent.attributes then
    let vdcl = List.find (fun v -> v.vname = name) parent.attributes in
    Var(vdcl), vdcl.vtype
  else
  if List.exists (fun f -> match f with
                      BasicFunc(x) -> x.fname = name
                    | AssertFunc(x) -> x.aname = name) parent.methods then
    let fdcl = search_func_in_child parent actuals name in
    let f_typ = (match fdcl with
                     BasicFunc(x) -> x.ftype
                   | AssertFunc(x) -> Bool) in
    Fun(fdcl), f_typ
  else
    raise Not_found

let rec search_func_in_parent scope actuals name =
  let rec helper = function
      [] ->
        (match scope.parent with
            Some(parent) -> search_func_in_parent parent actuals name
          | None -> raise Not_found)
    | f :: rest ->
        let formals = (match f with
                            BasicFunc(x) -> x.formals
                          | AssertFunc(x) -> x.aformals)
        in
        if List.length formals = List.length actuals then
          if verify_args_signature formals actuals then
            f
          else
            helper rest
        else
          helper rest
  in
  helper scope.functions

let rec search_field_local_first scope actuals name =
  let fe_is_v =
    List.exists (fun x -> x.vname = name) scope.variables
  and fe_is_f =
    List.exists (fun x -> match x with
                   BasicFunc(b) -> b.fname = name
                 | AssertFunc(a) -> a.aname = name )
                scope.functions
  and fe_is_g =
    List.exists (fun x -> x.gname = name) scope.groups
  in
  if fe_is_v then
    let vdecl =
      List.find (fun v -> v.vname = name) scope.variables
    in
    Var(vdecl)
  else if fe_is_f then
    let rec helper = function
        [] ->
            (match scope.parent with
                Some(parent) -> search_func_in_parent parent actuals name
              | None -> raise (SemError ("Function name " ^ name ^ " exists in scope " ^
                               "but actuals signature not matched")))
      | f :: rest ->
        let n, formals =
          (match f with
             BasicFunc(x) -> x.fname, x.formals
           | AssertFunc(x) -> x.aname, x.aformals)
        in
        if n = name &&
           List.length formals = List.length actuals &&
           verify_args_signature formals actuals then
            f
        else
          helper rest
    in
    let fdecl = helper scope.functions in
    Fun(fdecl)
  else if fe_is_g then
    let gdecl =
      List.find (fun g -> g.gname = name) scope.groups
    in
    Grp(gdecl)
  else
    match scope.parent with
        Some(parent) -> search_field_local_first parent actuals name
      | _ -> raise Not_found

let rec check_field env actuals = function
    Ast.Id(name) ->
      let dcl =
        try
          search_field_local_first env.scope actuals name
        with Not_found ->
          raise (SemError("Undeclared identifier: " ^ name))
      in
      let typ = match dcl with
          Var(v) -> v.vtype
        | Fun(x) -> (match x with
            BasicFunc(f) -> f.ftype
          | AssertFunc(a) -> Void)
        | Grp(g) -> Group(g.gname)
      in
      dcl, typ
  | Ast.FieldCall(fe, name) ->
    let parent, _ = check_field env [] fe in
      (match parent with
          Grp(par) ->
            (try
              find_child par actuals name
            with Not_found ->
              raise (SemError("Undeclared child identifier: " ^ name)))
        | _ ->
            raise (SemError("Parent is either a function or variable, not a group")))

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
      Field(Var(vdecl)), typ
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

let rec check_listlit env = function
    Ast.Elems(elems_list) ->
      let el = check_elems_list env elems_list in
      let e, typ = List.hd el in
      let typ = verify_elems_list_type env typ el in
      Elems(el), List_t(typ)
  | Ast.List(ll_list) ->
      let list_of_list_with_typ = List.map (check_listlit env) ll_list in
      let first_list, typ = List.hd list_of_list_with_typ in
      let typ = verify_list_of_list_type env typ list_of_list_with_typ in
      let list_of_list = List.map (fun (l, _) -> l) list_of_list_with_typ in
      List(list_of_list), List_t(typ)
  | Ast.EmptyList ->
      EmptyList, List_t(Void)

let ast_op_to_sast_op = function
    Ast.Add -> Add
  | Ast.Sub -> Sub
  | Ast.Mult -> Mult
  | Ast.Div -> Div
  | Ast.Equal -> Equal
  | Ast.Neq -> Neq
  | Ast.Less -> Less
  | Ast.Leq -> Leq
  | Ast.Greater -> Greater
  | Ast.Geq -> Geq
  | Ast.Mod -> Mod
  | Ast.And -> And
  | Ast.Or -> Or

let require_bool e msg = match e with
    _, Bool -> ()
  | _, _ -> raise (SemError msg)

let require_int e msg = match e with
    _, Int -> ()
  | _, _ -> raise (SemError msg)

let require_same e1 e2 msg =
  let _, t1 = e1
  and _, t2 = e2 in
  if t1 = t2 then
    ()
  else raise (SemError msg)

let require_integer_list l msg = match l with
    _, List_t(typ) ->
      (match typ with
         Int -> ()
       | _ -> raise (SemError msg))
  | _, _ -> raise (SemError msg)

let rec require_parent pname fe msg = match fe with
    Grp(p) ->
      if p.gname = pname then
        ()
      else
        (match p.extends with
           Some(gp) -> require_parent pname (Grp(gp)) msg
         | _ -> raise (SemError msg))
  | _ -> raise (SemError msg)

let rec verify_args_helper f_typ a_typ = match f_typ, a_typ with
    [], [] -> ()
  | f_hd :: f_rest, a_hd :: a_rest ->
      if f_hd = a_hd then
        verify_args_helper f_rest a_rest
      else
        raise (SemError ("Formal type " ^ string_of_t f_hd ^ " " ^
                         "does not match actual type " ^ string_of_t a_hd))
  | _, [] ->
      raise (SemError ("Formal and actual argument lengths do not match"))
  | [], _ ->
      raise (SemError ("Formal and actual argument lengths do not match"))

let verify_args formals actuals =
  let f_typ = List.map (fun v -> v.vtype) formals
  and a_typ = List.map (fun (_, typ) -> typ) actuals
  in
  verify_args_helper f_typ a_typ

let rec check_expr env = function
    Ast.IntLiteral(i) -> IntLiteral(i), Int
  | Ast.StrLiteral(s) -> StrLiteral(s), Str
  | Ast.ListLiteral(ll) ->
      let l, typ = check_listlit env ll in
      ListLiteral(l), typ
  | Ast.BoolLiteral(b) ->
      (match b with
          Ast.True -> BoolLiteral(True), Bool
        | Ast.False -> BoolLiteral(False), Bool)
  | Ast.VoidLiteral -> VoidLiteral, Void
  | Ast.Field(fe) ->
      let f, typ = check_field env [] fe in
      Field(f), typ
 | Ast.Binop(e1, op, e2) ->
      let e1 = check_expr env e1
      and e2 = check_expr env e2 in
      if op = Ast.Add || op = Ast.Sub || op = Ast.Mult ||
         op = Ast.Div || op = Ast.Mod then
            ((require_int e1 "Left operand must be integer";
             require_int e2 "Right operand must be integer";
             let op = ast_op_to_sast_op op in
             Binop(e1, op, e2), Int))
      else if op = Ast.Less || op = Ast.Leq ||
              op = Ast.Greater || op = Ast.Geq then
            ((require_int e1 "Left operand must be integer";
              require_int e2 "Right operand must be integer";
              let op = ast_op_to_sast_op op in
              Binop(e1, op, e2), Bool))
      else if op = Ast.Equal || op = Ast.Neq then
        (require_same e1 e2 "Left and right operands in comparison must be equal";
         let op = ast_op_to_sast_op op in
         Binop(e1, op, e2), Bool)
      else (* op = Ast.And || op = Ast.Or *)
        (require_bool e1 "Left opreand must be boolean";
         require_bool e2 "Right operand must be boolean";
         let op = ast_op_to_sast_op op in
         Binop(e1, op, e2), Bool)
  | Ast.Assign(fd, e) ->
      let field, tf = check_field env [] fd
      and e = check_expr env e in
      let _, te = e in
      if tf = te then
        Assign(field, e), tf
      else
        raise (SemError ("Types differ in assignment expression; expected: " ^
                         string_of_t tf))
  | Ast.Call(fd, el) ->
      let actuals = List.map (check_expr env) el in
      let fd, typ = check_field env actuals fd in
      (match fd with
           Fun(f) ->
             (match f with
                  BasicFunc(bf) ->
                    verify_args bf.formals actuals
                | AssertFunc(af) ->
                    verify_args af.aformals actuals);
             Call(f, actuals), typ
         | Grp(g) -> raise (SemError ("Not callable: " ^ g.gname))
         | Var(v) -> raise (SemError ("Not callable: " ^ v.vname)))
  | Ast.Element(e1, e2) ->
      let e1 = check_expr env e1
      and e2 = check_expr env e2 in
      let _, t1 = e1 in
      require_integer_list e2 "List of integers expected";
      (match t1 with
          List_t(typ) -> Element(e1, e2), typ
        | _ -> raise (SemError ("Expression not subscriptable")))
  | Ast.Uminus(e) ->
      let e = check_expr env e in
      require_int e "Operand must be integer";
      Uminus(e), Int
  | Ast.Not(e) ->
      let e = check_expr env e in
      require_bool e "Operand must be boolean";
      Not(e), Bool
  | Ast.Noexpr ->
      Noexpr, Void
  | Ast.Remove(fd1, fd2, ll) ->
      let fd1, _ = check_field env [] fd1
      and fd2, _ = check_field env [] fd2
      and ll, ll_typ = check_listlit env ll in
      require_parent "Board" fd1 "Board (sub)group expected";
      require_parent "Piece" fd2 "Piece (sub)group expected";
      require_integer_list (ll, ll_typ) "List of integers expected";
      Remove(fd1, fd2, ll), Bool
  | Ast.Place(fd1, fd2, ll) ->
      let fd1, _ = check_field env [] fd1
      and fd2, _ = check_field env [] fd2
      and ll, ll_typ = check_listlit env ll in
      require_parent "Piece" fd1 "Piece (sub)group expected";
      require_parent "Board" fd2 "Board (sub)group expected";
      require_integer_list (ll, ll_typ) "List of integers expected";
      Remove(fd1, fd2, ll), Bool

let rec verify_expr_list_type env typ = function
    [] -> typ
  | (a, b) :: rest ->
      if b = typ then
        verify_expr_list_type env typ rest
      else
        raise (SemError ("List of expressions are not all of same type: " ^
                         string_of_t typ))

let check_init env = function
    Ast.ExprInit(e) -> Some(check_expr env e)
  | Ast.NoInit -> None

let rec find_turn_name (scope : symbol_table) name =
  try
    List.find (fun t -> t = name) scope.turns
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_turn_name parent name
    | _ -> raise Not_found

let rec check_stmt env = function
    Ast.Block(sl) ->
      let scope' =
        { parent = Some(env.scope);
          variables = [];
          functions = [];
          groups = [];
          turns = [] } in
      let env' =
        { env with scope = scope'; } in
      let sl = List.map (fun s -> check_stmt env' s) sl in
      Block(scope', sl)
  | Ast.Expr(e) -> Expression(check_expr env e)
  | Ast.Pass(s, e) ->
      let turn_name =
        try
          find_turn_name env.scope s
        with Not_found ->
          raise (SemError ("Turn name not found: " ^ s))
      in
      let dummy_turn_func =
        { ftype = Void;
          fname = turn_name;
          formals = [];
          locals = [];
          body = [];
          turns_func = true }
      in
      let e = check_expr env e in
      require_int e "Must pass to an integer player.";
      Pass(BasicFunc(dummy_turn_func), e)
  | Ast.Return(e) ->
      let e = check_expr env e in
      let _, typ = e in
      if env.return_type <> typ then
        raise (SemError ("Return types do not match; expected: " ^
                         string_of_t env.return_type))
      else
        Return(e)
  | Ast.Break ->
      if env.in_loop = false then
        raise (SemError ("Break outside of loop"))
      else
        Break
  | Ast.Continue ->
      if env.in_loop = false then
        raise (SemError ("Continue outside of loop"))
      else
        Continue
  | Ast.End -> End
  | Ast.If(e, s1, s2) ->
      let e = check_expr env e in
      require_bool e "Predicate of if must be boolean";
      If(e, check_stmt env s1, check_stmt env s2)
  | Ast.For(vd, el, s) ->
      let decl =
      { vname = vd.Ast.vname;
        vtype = id_type_to_t vd.Ast.vtype;
        vinit = check_init env vd.Ast.vinit}
      and el = List.map (check_expr env) el in
      let _, t_el = List.hd el in
      let t_vd = decl.vtype
      and t_el = verify_expr_list_type env t_el el in
      if t_vd <> t_el then
        raise (SemError ("For loop elements and loop variable must be " ^
                         "the same type. Variable type: " ^ string_of_t t_vd ^
                         "; List type: " ^ string_of_t t_el))
      else
        let scope' =
          { parent = Some(env.scope);
          variables = [];
          functions = [];
          groups = [];
          turns = [] } in
        let env' = { env with scope = scope'; in_loop = true } in
        scope'.variables <- decl :: scope'.variables;
        For(decl, el, check_stmt env' s)
  | Ast.While(e, s) ->
      let e = check_expr env e in
      let scope' =
        { parent = Some(env.scope);
          variables = [];
          functions = [];
          groups = [];
          turns = [] } in
      let env' = { env with scope = scope'; in_loop = true } in
      require_bool e "While loop predicate must be Boolean";
      While(e, check_stmt env' s)

let rec check_for_begin(turns_section) = match turns_section with
    [] -> false
  | Ast.BasicFunc(f) :: rest ->
      if f.Ast.fname = "begin" then
        true
      else
        check_for_begin(rest)
  | Ast.AssertFunc(f) :: rest -> check_for_begin(rest)

let require_no_init = function
    Ast.NoInit -> ()
  | Ast.ExprInit(e) ->
      raise (SemError "Function formal arguments cannot have default values.")

let check_vdcl_helper env v init_ok =
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
    decl

let check_formal env v =
  let decl = check_vdcl_helper env v false in
  let decl = {decl with vinit = None} in
  require_no_init v.Ast.vinit;
  env.scope.variables <- decl :: env.scope.variables;
  decl

let check_vdcl env v =
  let decl = check_vdcl_helper env v true in
  env.scope.variables <- decl :: env.scope.variables;
  decl

let verify_if_arg_types_equal new_fun f =
  let rec helper x y = match x, y with
      [], [] ->
        (match new_fun with
            Ast.BasicFunc(nf) ->
              raise (SemError ("Function name and type signature already declared: " ^
                               nf.Ast.fname))
          | Ast.AssertFunc(nf) ->
              raise (SemError ("Function name and type signature already declared: " ^
                               nf.Ast.fname)))
    | a :: xrest, b :: yrest ->
        let ta = a.Ast.vtype
        and tb = b.vtype in
        if id_type_to_t ta = tb then
          helper xrest yrest
        else
          ()
    | _, [] -> ()
    | [], _ -> ()
  in
  match new_fun, f with
      Ast.BasicFunc(nf), BasicFunc(bf) ->
        helper nf.Ast.formals bf.formals
    | Ast.AssertFunc(nf), AssertFunc(af) ->
        helper nf.Ast.formals af.aformals
    | Ast.BasicFunc(nf), _ ->
        raise (SemError ("Declaring a basic function but an assert function " ^
                         "with same name already exists: " ^ nf.Ast.fname))
    | Ast.AssertFunc(nf), _ ->
        raise (SemError ("Declaring an assert function but a basic function " ^
                         "with same name already exists: " ^ nf.Ast.fname))

let rec verify_if_func_declared new_fun = function
    [] -> ()
  | hd :: rest ->
      (match new_fun, hd with
          Ast.BasicFunc(nf), BasicFunc(f) ->
            if nf.Ast.fname = f.fname then
              verify_if_arg_types_equal new_fun (BasicFunc(f))
            else
              verify_if_func_declared new_fun rest
        | Ast.AssertFunc(nf), AssertFunc(f) ->
            if nf.Ast.fname = f.aname then
              verify_if_arg_types_equal new_fun (AssertFunc(f))
            else
              verify_if_func_declared new_fun rest
        | Ast.BasicFunc(nf), AssertFunc(f) ->
            if nf.Ast.fname = f.aname then
              raise (SemError ("Function already declared in scope: " ^ f.aname))
            else
              verify_if_func_declared new_fun rest
        | Ast.AssertFunc(nf), BasicFunc(f) ->
            if nf.Ast.fname = f.fname then
              raise (SemError ("Function already declared in scope: " ^ f.fname))
            else
              verify_if_func_declared new_fun rest)

let check_basic_func env in_turn_section (f : Ast.basic_func_decl) =
    let scope' =
      { parent = Some(env.scope);
        variables = [];
        functions = [];
        groups = [];
        turns = [] } in
    let env' =
      { env with scope = scope';
        return_type = id_type_to_t f.Ast.ftype; } in
    let fl = List.map (fun v -> check_formal env' v) f.Ast.formals in
    let ll = List.map (fun dcl -> check_vdcl env' dcl) f.Ast.locals in
    let sl = List.map (fun s -> check_stmt env' s) f.Ast.body in
    let fdecl =
      BasicFunc({ ftype = id_type_to_t f.Ast.ftype;
                  fname = f.Ast.fname;
                  formals = fl;
                  locals = ll;
                  body = sl;
                  turns_func = in_turn_section })
    in
    env.scope.functions <- fdecl :: env.scope.functions;
    fdecl

let check_assert_func env in_turn_section (f : Ast.assert_decl) =
  let already_declared =
    List.exists (fun x -> match x with
                   BasicFunc(b) -> b.fname = f.Ast.fname
                 | AssertFunc(a) -> a.aname = f.Ast.fname )
                env.scope.functions
  in
  if already_declared then
    raise (SemError ("Function name previously declared in scope: " ^
                     f.Ast.fname))
  else
    let scope' =
      { parent = Some(env.scope);
        variables = [];
        functions = [];
        groups = [];
        turns = [] } in
    let env' =
      { env with scope = scope';
        return_type = Bool; } in
    let fl = List.map (fun v -> check_formal env' v) f.Ast.formals in
    let ll = List.map (fun dcl -> check_vdcl env' dcl) f.Ast.locals in
    let sl = List.map (fun s -> check_stmt env' s) f.Ast.body in
    let fdecl =
      AssertFunc({ aname = f.Ast.fname;
                   aformals = fl;
                   alocals = ll;
                   abody = sl ;
                   a_turns_func = in_turn_section })
    in
    env.scope.functions <- fdecl :: env.scope.functions;
    fdecl

let check_function env in_turn_section new_fun =
  verify_if_func_declared new_fun env.scope.functions;
  match new_fun with
      Ast.BasicFunc(f) ->
        check_basic_func env in_turn_section f
    | Ast.AssertFunc(f) ->
        check_assert_func env in_turn_section f

let find_init_func methods =
    List.find (fun x -> match x with
                    BasicFunc(f) -> f.fname = "__init__"
                  | AssertFunc(f) -> false) methods

let verify_extends parent par_actuals init_opt =
  match parent with
      Some(p) ->
        (match par_actuals with
            Some(el) ->
              (match init_opt with
                  Some(init) ->
                    raise (SemError ("Defined child constructor while also using parent's constructor"))
                | None ->
                    let par_init = try
                      find_init_func p.methods
                    with Not_found ->
                      raise (SemError("Constructor function not found in parent"))
                    in
                    let par_init = match par_init with
                        BasicFunc(f) -> f
                      | AssertFunc(f) ->
                          raise (SemError("Parent constructor cannot be an assert function"))
                    in
                    if List.length par_init.formals = List.length el then
                      verify_args par_init.formals el
                    else
                      raise (SemError("Number of constrctur variables for parent's constructor does not match")))
          | None ->
              match init_opt with
                  Some(init) -> ()
                | None -> raise (SemError ("Constructor function not found")))
    | None ->
        (match init_opt with
            Some(init) -> ()
          | None -> raise (SemError("No constructor function")))

let rec verify_single_attrib par_attr v = match par_attr with
    [] -> Some(v)
  | p :: rest ->
      if p.vname = v.vname then
        if p.vtype = v.vtype then
          None
        else
          raise (SemError("Cannot change type of inherited attribute"))
      else
        verify_single_attrib rest v

let rec verify_attributes par_attr = function
    [] -> []
  | v :: rest ->
      (match verify_single_attrib par_attr v with
          None -> verify_attributes par_attr rest
        | Some(var) -> var :: verify_attributes par_attr rest)

let rec check_group env g =
  let parent = match g.Ast.extends with
      Some(fe) ->
        let par, _ = check_field env [] fe in
        (match par with
            Grp(p) -> Some(p)
          | _ -> raise (SemError ("Parent is not a group")))
    | None -> None
  in
  let par_actuals = match g.Ast.par_actuals with
      Some(el) -> Some(List.map (check_expr env) el)
    | None -> None
  in
  let scope' =
      { parent = Some(env.scope);
        variables = [];
        functions = [];
        groups = [];
        turns = [] } in
    let env' =
      { env with scope = scope';
        return_type = Void; } in
  let attribs = List.map (check_vdcl env') g.Ast.attributes in
  let attribs =
    (match parent with
       Some(par) -> verify_attributes par.attributes attribs
     | None -> attribs) in
  let methods = List.map (check_function env' false) g.Ast.methods in
  let init_opt = try
    Some(find_init_func methods)
  with Not_found ->
    None
  in
  verify_extends parent par_actuals init_opt;
  let gdecl =
    { gname = g.Ast.gname;
      extends = parent;
      par_actuals = par_actuals;
      attributes = attribs;
      methods = methods }
  in
  env.scope.groups <- gdecl :: env.scope.groups;
  gdecl

let check_setup env setup_section =
  let vars, funcs, groups = setup_section in
  List.map (check_vdcl env) vars,
  List.map (check_function env false) funcs,
  List.map (check_group env) groups

let built_in_funcs =
  [BasicFunc({ ftype = Void;
               fname = "print";
               formals = [{ vname = "print_arg";
                            vtype = Str;
                            vinit = None}];
               locals = [];
               body = [];
               turns_func = false });
  BasicFunc({ ftype = Void;
               fname = "print";
               formals = [{ vname = "print_arg";
                            vtype = Bool;
                            vinit = None}];
               locals = [];
               body = [];
               turns_func = false });
  BasicFunc({ ftype = Void;
               fname = "print";
               formals = [{ vname = "print_arg";
                            vtype = Int;
                            vinit = None}];
               locals = [];
               body = [];
               turns_func = false })]

let rec gather_turn_names env = function
    [] -> ()
  | Ast.BasicFunc(t) :: rest ->
      env.scope.turns <- t.Ast.fname :: env.scope.turns;
      gather_turn_names env rest
  | Ast.AssertFunc(t) :: rest ->
      env.scope.turns <- t.Ast.fname :: env.scope.turns;
      gather_turn_names env rest

let check_turns env turns_section =
  if check_for_begin(turns_section) = false then
    raise (SemError "No begin() function in @turns section")
  else
    gather_turn_names env turns_section;
    List.map (check_function env true) turns_section

let fix_pass_stmts_helper turns = function
    Pass(s, e) ->
      let name = (match s with
                    BasicFunc(x) -> x.fname
                  | AssertFunc(x) -> x.aname)
      in
      let fdcl =
        try
          List.find
            (fun f -> match f with
               BasicFunc(x) -> x.fname = name
             | AssertFunc(x) -> x.aname = name)
          turns
        with Not_found ->
          raise (SemError ("Turns function declaration not found in pass statement: " ^ name ))
      in
      Pass(fdcl, e)
  | s -> s

let fix_pass_stmts turns = function
    BasicFunc(f) ->
    let new_f =
      {f with body = List.map (fix_pass_stmts_helper turns) f.body}
    in
    BasicFunc(new_f)
  | AssertFunc(f) ->
    let new_f =
      {f with abody = List.map (fix_pass_stmts_helper turns) f.abody}
    in
    AssertFunc(new_f)

let check_program (program : Ast.program) =
  let symbols =
    { parent = None ;
      variables = [];
      functions = built_in_funcs;
      groups = [];
      turns = [] } in
  let env =
    { scope = symbols;
      return_type = Void;
      in_loop = false } in
  let setup_section, turns_section = program in
  let setup_section = check_setup env setup_section in
  let turns_section = check_turns env turns_section in
  let turns_section = List.map (fix_pass_stmts turns_section) turns_section in
  setup_section, turns_section

