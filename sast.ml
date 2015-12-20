open Types

exception SemError of string

type counter = {
  mutable i : int
}

let count = { i = 0 }

let rec string_of_t = function
    Int -> "int"
  | Bool -> "bool"
  | Str -> "str"
  | Void -> "void"
  | List_t(vt) ->
      "list[" ^ string_of_t vt ^ "]"
  | Group(s, _) -> "group " ^ s

let rec id_type_to_t = function
    Ast.Int -> Int
  | Ast.Bool -> Bool
  | Ast.Str -> Str
  | Ast.Void -> Void
  | Ast.List(id_typ) -> List_t(id_type_to_t id_typ)
  | Ast.Group(s) -> Group(s, None)

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

let verify_args_signature fdcl formals actuals =
  let rec helper check_types flist alist = match flist, alist with
      [], [] -> true
    | f :: frest, a :: arest ->
      let tf = f.vtype
      and _, ta = a in
      if tf = ta || not check_types then
        helper check_types frest arest
      else
        (match tf, ta with
           Group(form_name, _), Group(act_name, _) ->
            (* if form_name = act_name then *)
              helper check_types frest arest
            (* else
              false *)
          | _, _ -> false)
    | _ :: _, [] -> false
    | [], _ :: _ -> false
  in
  let fname, is_built_in =
    (match fdcl with
        BasicFunc(f) -> f.fname, f.f_is_built_in
      | AssertFunc(f) -> f.aname, f.a_is_built_in)
  in
  let check_formal_types =
    if fname = "print" && is_built_in then
      let formal_type =
        (match fdcl with
            BasicFunc(f) -> List.hd f.formals
          | _ -> raise (SemError ("Internal error: built-in print function cannot be an assert function")))
      in
      (match formal_type.vtype with
          Group("", _) -> false
        | _ -> true)
    else
      true
  in
  helper check_formal_types formals actuals

let rec search_func_in_child (parent : group_decl) actuals name =
  let rec helper = function
      [] -> raise Not_found
    | f :: rest ->
        let fname, formals =
          (match f with
              BasicFunc(x) -> x.fname, x.formals
            | AssertFunc(x) -> x.aname, x.aformals)
        in
        if name = fname &&
           List.length formals = List.length actuals then
          if verify_args_signature f formals actuals then
            f
          else
            helper rest
        else
          helper rest
  in
  helper parent.methods

let search_func_in_scope scope actuals name =
  let rec helper = function
      [] -> raise (SemError ("Function name " ^ name ^ " exists in group scope " ^
                             "but actuals signature not matched"))
    | f :: rest ->
        let fname, formals =
          (match f with
              BasicFunc(x) -> x.fname, x.formals
            | AssertFunc(x) -> x.aname, x.aformals)
        in
        if name = fname &&
          List.length formals = List.length actuals then
          if verify_args_signature f formals actuals then
            f
          else
            helper rest
        else
          helper rest
  in
  helper scope.functions

let rec find_child_in_group_def env (parent : group_decl) actuals name =
  if List.exists (fun v -> v.vname = name) parent.attributes then
    let vdcl = List.find (fun v -> v.vname = name) parent.attributes in
    Var(vdcl), vdcl.vtype
  else
  if List.exists (fun f -> match f with
                      BasicFunc(x) -> x.fname = name
                    | AssertFunc(x) -> x.aname = name) parent.methods then
    try
      let fdcl = search_func_in_child parent actuals name in
      let f_typ = (match fdcl with
                       BasicFunc(x) -> x.ftype
                     | AssertFunc(x) -> Bool) in
      Fun(fdcl), f_typ
    with Not_found ->
      (match parent.extends with
          None -> raise Not_found
        | Some(g) -> find_child_in_group_def env g actuals name)
  else
    (match parent.extends with
        None -> raise Not_found
      | Some(g) -> find_child_in_group_def env g actuals name)

let rec find_child env (par_instance : var_decl) actuals name =
  let class_name =
    (match par_instance.vtype with
       Group(s, _) -> s
     | _ -> raise (SemError ("DOT operator does not work with non-group variable: " ^ par_instance.vname)))
  in
  let parent =
    try
      find_group env.scope class_name
    with Not_found ->
      raise (SemError ("Group definition not found: " ^ class_name))
  in
  let child, child_typ =
    try
      find_child_in_group_def env parent actuals name
    with Not_found ->
        raise (SemError ("Child of " ^ parent.gname ^ " not found: " ^ name ^
                         ";\nActual types: " ^ String.concat ", " (List.map (fun (_, typ) -> string_of_t typ) actuals)))
  in
  (match child with
      Var(v) -> Attrib(par_instance, v), child_typ
    | Fun(f) -> Method(par_instance, f), child_typ
    | _ -> raise (SemError ("Child is not a variable or function")))

let find_this_child env actuals name =
  let info =
    (match env.partial_group_info with
       None -> (raise (SemError "'this' field call outside of group definition"))
     | Some(info) -> info) in
  let this_dummy =
    { vname = "this"; vtype = Group(info.group_name, None);
      vinit = None; vloop = false } in
  let scope = info.symbols in
  if List.exists (fun v -> v.vname = name) scope.variables then
    let vdcl = List.find (fun v -> v.vname = name) scope.variables in
    Attrib(this_dummy, vdcl), vdcl.vtype
  else
  if List.exists (fun f -> match f with
                    BasicFunc(x) -> x.fname = name
                  | AssertFunc(x) -> x.aname = name) scope.functions then
    let fdcl = search_func_in_scope scope actuals name in
    let f_typ = (match fdcl with
                   BasicFunc(x) -> x.ftype
                 | AssertFunc(x) -> Bool) in
      Method(this_dummy, fdcl), f_typ
  else
    (match info.par with
        None -> raise Not_found
      | Some(p) ->
          let child, child_typ =
            try
              find_child_in_group_def env p actuals name
            with Not_found ->
              raise (SemError ("Child of 'this' not found: " ^ name))
          in
          (match child with
              Var(v) -> Attrib(this_dummy, v), child_typ
            | Fun(f) -> Method(this_dummy, f), child_typ
            | _ -> raise (SemError ("Child is not a variable or function"))))

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
          if verify_args_signature f formals actuals then
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
           verify_args_signature f formals actuals then
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
          | AssertFunc(a) -> Bool)
        | Grp(g) -> Group(g.gname, None)
        | This -> raise (SemError("Internal error: 'this' keyword match with Ast.Id"))
        | _ -> raise (SemError "Internal error: Ast.Id matched with Attrib or Method")
      in
      dcl, typ
  | Ast.This ->
      let gname =
        (match env.partial_group_info with
            None -> raise (SemError("'this' keyword used outside of group declaration"))
          | Some(info) -> info.group_name)
      in
      This, Group(gname, None)
  | Ast.FieldCall(fe, name) ->
    let parent, _ = check_field env [] fe in
      (match parent with
          Var(par) ->
            (try
              find_child env par actuals name
            with Not_found ->
              raise (SemError("Undeclared child identifier: " ^ name)))
        | This ->
            (try
              find_this_child env actuals name
            with Not_found ->
              raise (SemError("Undeclared 'this' child identifier: " ^ name)))
        | _ ->
            raise (SemError("Parent is either a function or variable, not a group")))

let create_ll_name scope =
  scope.ll_count <- scope.ll_count + 1;
  "__ll__" ^ string_of_int scope.ll_count

let create_elem_name scope =
  scope.elem_count <- scope.elem_count + 1;
  "__elem__" ^ string_of_int scope.elem_count

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

let rec require_parent_helper pname gdcl msg =
  if gdcl.gname = pname then
    ()
  else
    (match gdcl.extends with
       Some(gp) -> require_parent_helper pname gp msg
     | _ -> raise (SemError msg))

let rec require_parent env pname fe msg = match fe with
    Var(v) ->
      (match v.vtype with
          Group(s, _) ->
            let gdcl =
              try
                find_group env.scope s
              with Not_found ->
                raise (SemError ("require_parent did not find group: " ^ pname))
            in
            require_parent_helper pname gdcl msg
        | _ -> raise (SemError ("Variable is not a group: " ^ v.vname)))
  | _ -> raise (SemError ("Field expr. is not a variable"))

let rec verify_args_helper f_typ a_typ check_types = match f_typ, a_typ with
    [], [] -> ()
  | f_hd :: f_rest, a_hd :: a_rest ->
      if f_hd = a_hd then
        verify_args_helper f_rest a_rest check_types
      else if not check_types then
        ()
      else
        (match f_hd, a_hd with
            Group(_, _), Group(_, _) -> ()
          | _, _ ->
            raise (SemError ("Formal type " ^ string_of_t f_hd ^ " " ^
                             "does not match actual type " ^ string_of_t a_hd)))
  | _, [] ->
      raise (SemError ("Formal and actual argument lengths do not match"))
  | [], _ ->
      raise (SemError ("Formal and actual argument lengths do not match"))

let verify_args fdcl formals actuals =
  let f_typ = List.map (fun v -> v.vtype) formals in
  let a_typ = List.map (fun (_, typ) -> typ) actuals in
  let fname, is_built_in =
    (match fdcl with
        BasicFunc(f) -> f.fname, f.f_is_built_in
      | AssertFunc(f) -> f.aname, f.a_is_built_in)
  in
  if fname = "print" && is_built_in then
    let formal_type =
      (match fdcl with
          BasicFunc(f) -> List.hd f.formals
        | _ -> raise (SemError ("Internal error: built-in print function cannot be an assert function")))
    in
    let check_formal_types =
      (match formal_type.vtype with
          Group("", _) -> false
        | _ -> true)
    in
    verify_args_helper f_typ a_typ check_formal_types
  else
    verify_args_helper f_typ a_typ true

let rec verify_elems_list_type env typ = function
    [] -> typ
  | (a, b) :: rest ->
      if b = typ then
        verify_elems_list_type env typ rest
      else
        raise (SemError ("List elements are not all of same type: " ^
                         string_of_t typ))

let rec check_single_elem env (detail, typ) = match detail with
    IntLiteral(i, _) ->
      let name = create_elem_name env.scope in
      IntLiteral(i, name), typ
  | StrLiteral(s, _) ->
      let name = create_elem_name env.scope in
      StrLiteral(s, name), typ
  | ListLiteral(ll) ->
      raise (SemError ("A list literal cannot be a list element."))
  | BoolLiteral(bl, _) ->
      let name = create_elem_name env.scope in
      (match bl with
          True -> BoolLiteral(True, name), typ
        | False -> BoolLiteral(False, name), typ)
  | Field(fe) ->
      (match fe with
          Var(_) -> detail, typ
        | Attrib(_, _) -> detail, typ
        | Method (_, _) -> detail, typ
        | _ ->
          raise (SemError ("A list element is not a variable, " ^
                           "attribute, or method")))
  | Binop(e1, op, e2) ->
      raise (SemError ("A list element cannot be binary expression."))
  | Assign(fe, e) ->
      raise (SemError ("A list element cannot be assign expression."))
  | Call(vd_opt, fd, el) ->
      raise (SemError ("A list element cannot be call expression."))
  | Uminus(e) ->
      raise (SemError ("A list element cannot be unary minus expression."))
  | Not(e) ->
      raise (SemError ("A list element cannot be not expression."))
  | Noexpr ->
      raise (SemError ("A list element cannot be an empty expression."))
  | Remove(_) ->
      raise (SemError ("A list element cannot be a remove expression."))
  | Place(_, _, _) ->
      raise (SemError ("A list element cannot be a place expression."))
  | _ -> detail, typ

and check_listlit env = function
    Ast.Elems(elems_list) ->
      let el = List.map (check_expr env) elems_list in
      let el = List.map (check_single_elem env) el in
      let e, typ = List.hd el in
      let typ = verify_elems_list_type env typ el in
      let name = create_ll_name env.scope in
      Elems(el,name), List_t(typ)
  | Ast.EmptyList ->
      EmptyList, List_t(Void)

and check_expr env = function
    Ast.IntLiteral(i) -> IntLiteral(i, ""), Int
  | Ast.StrLiteral(s) -> StrLiteral(s, ""), Str
  | Ast.ListLiteral(ll) ->
      let l, typ = check_listlit env ll in
      ListLiteral(l), typ
  | Ast.BoolLiteral(b) ->
      (match b with
          Ast.True -> BoolLiteral(True, ""), Bool
        | Ast.False -> BoolLiteral(False, ""), Bool)
  | Ast.VoidLiteral -> VoidLiteral, Void
  | Ast.Field(fe) ->
      let f, typ = check_field env [] fe in
      Field(f), typ
 | Ast.Binop(e1, op, e2) ->
      let e1 = check_expr env e1
      and e2 = check_expr env e2 in
      if op = Ast.Add || op = Ast.Sub || op = Ast.Mult ||
         op = Ast.Div || op = Ast.Mod then
          let _, t1 = e1 and _, t2 = e2 in
          let op = ast_op_to_sast_op op in
          match t1, t2 with
              Str, Str ->
                Binop(e1, op, e2), Str
            | Int, Int -> Binop(e1, op, e2), Int
            | _, _ ->
              raise (SemError ("Additiona operation requires two integer " ^
                               "or two string operands."))
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
                    verify_args f bf.formals actuals
                | AssertFunc(af) ->
                    verify_args f af.aformals actuals);
             Call(None, f, actuals), typ
         | This -> raise (SemError ("Not callable: 'this'"))
         | Var(v) -> raise (SemError ("Not callable: " ^ v.vname))
         | Attrib(v1, v2) -> raise (SemError ("Not callable: " ^ v1.vname ^ "." ^ v2.vname))
         | Grp(g) ->
            let par =
              { vname = g.gname ; vtype = Group(g.gname, None);
                vinit = None; vloop = false }
            in
            let init =
              List.find (fun f -> match f with
                            BasicFunc(x) -> x.fname = "__init__"
                          | AssertFunc(_) -> false) g.methods
            in
            let formals = match init with
                BasicFunc(x) -> x.formals
              | AssertFunc(_) -> raise (SemError ("Internal error: __init__ is an assert function"))
            in
            verify_args init formals actuals;
            Call(Some(par), init, actuals), Group(g.gname, None)
         | Method(par, child) ->
              (match par.vtype with
                  Group(s, _) -> ()
                | _ -> raise (SemError ("Method call with parent that is not a group")));
             (match child with
                  BasicFunc(bf) ->
                    verify_args child bf.formals actuals
                | AssertFunc(af) ->
                    verify_args child af.aformals actuals);
             Call(Some(par), child, actuals), typ)
  | Ast.Element(e1, e2) ->
      let e1 = check_expr env e1
      and e2 = check_expr env e2 in
      let _, t1 = e1 in
      require_int e2 "Integer expected for element number";
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
  | Ast.Remove(fd1, ll) ->
      let board_name = match fd1 with
          Ast.Id(s) -> s
        | _ -> raise (SemError "Not implemented")
      in
      let toi_call = Ast.Call(Ast.FieldCall(Ast.Id(board_name), "toi"),
                              [Ast.ListLiteral(ll)])
      in
      let rmv_call = Ast.Call(Ast.FieldCall(Ast.Id(board_name), "remove"),
                              [toi_call])
      in
      let rmv_call = check_expr env rmv_call in
      let fd1, _ = check_field env [] fd1 in
      let checked_ll, ll_typ = check_listlit env ll in
      require_parent env "Board" fd1 "Board (sub)group expected";
      require_integer_list (checked_ll, ll_typ) "List of integers expected";
      Remove(rmv_call), Bool
  | Ast.Place(fd1, fd2, ll) ->
      let fd1, _ = check_field env [] fd1
      and fd2, _ = check_field env [] fd2
      and ll, ll_typ = check_listlit env ll in
      require_parent env "Piece" fd1 "Piece (sub)group expected";
      require_parent env "Board" fd2 "Board (sub)group expected";
      require_integer_list (ll, ll_typ) "List of integers expected";
      Place(fd1, fd2, ll), Bool

let rec verify_expr_list_type env typ = function
    [] -> typ
  | (a, b) :: rest ->
      if b = typ then
        verify_expr_list_type env typ rest
      else
        raise (SemError ("List of expressions are not all of same type: " ^
                         string_of_t typ))

(* let verify_group_cast env to_name from_name =
  let rec helper gdcl =
    if gdcl.gname = to_name then
      ()
    else
      (match gdcl.extends with
          Some(g) -> helper g
        | None -> raise (SemError ("Invalid group cast: " ^ to_name ^
                         " is not an ancestor of " ^ from_name)))
  in
  let from_grp = find_group env.scope from_name in
  helper from_grp *)

let check_init env v_name v_typ = function
    Ast.ExprInit(e) ->
      let e = check_expr env e in
      let _, typ = e in
      if v_typ = typ then
        Some(e)
      else
        (match v_typ, typ with
          Group(to_, _), Group(from, _) ->
            (* verify_group_cast env to_ from; *)
            Some(e)
         | _, _ ->
            raise (SemError ("Variable initiation type for identifer " ^ v_name ^
                             " does not match, expected: " ^ string_of_t v_typ)))
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
          turns = [];
          ll_count = 0;
          elem_count = 0 } in
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
          turns_func = true;
          group_method = "";
          f_is_built_in = false }
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
      let name, t_vd = vd.Ast.vname, id_type_to_t vd.Ast.vtype in
      let decl =
      { vname = name;
        vtype = t_vd;
        vinit = check_init env name t_vd vd.Ast.vinit;
        vloop = true }
      and el = List.map (check_expr env) el in
      let _, t_el = List.hd el in
      let t_el = verify_expr_list_type env t_el el in
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
          turns = [];
          ll_count = 0;
          elem_count = 0 } in
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
          turns = [];
          ll_count = 0;
          elem_count = 0 } in
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

let require_non_void v = match v.vtype with
    Void -> raise (SemError (v.vname ^ " declared with void type"))
  | _ -> ()

let verify_not_redeclaring scope name =
  let is_var_or_fun_in_scope =
    List.exists (fun x -> x.vname = name) scope.variables ||
    List.exists (fun x -> match x with
                   BasicFunc(f) -> f.fname = name
                 | AssertFunc(f) -> f.aname = name) scope.functions
  in
  let gdcl_opt =
    try
      Some(find_group scope name)
    with Not_found ->
      None
  in
  match gdcl_opt with
    Some(_) ->
      raise (SemError ("Cannot redeclare an identifier that is also a group " ^
                       "definition name: " ^ name))
  | None ->
    if is_var_or_fun_in_scope then
      raise (SemError ("Cannot redeclare an indentifer matching with a " ^
                       "variable or a function in current scope: " ^ name))
    else
      ()

let check_vdcl_helper env v init_ok =
  let name = v.Ast.vname in
  let t_vd = id_type_to_t v.Ast.vtype in
  let init = check_init env name t_vd v.Ast.vinit in
  let init =
    (match init with
        None -> init
      | _ ->
        if not init_ok then
          raise (SemError ("Initiation not allowed here for variable: " ^ name))
        else
          init)
  in
  let decl =
    { vname = name;
      vtype = t_vd;
      vinit = init;
      vloop = false }
  in
  verify_not_redeclaring env.scope v.Ast.vname;
  require_non_void decl;
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

let rec verify_implicit_return_basic_fun name ftyp body=
  let last_stmt =
    try
      List.hd (List.rev body)
    with Failure("hd") ->
      End (* empty body, so use non-Return dummy stmt *)
  in
  match last_stmt with
    Return(e) ->
      () (* Last statement is not an implicit return *)
  | Block(scope, sl) ->
      verify_implicit_return_basic_fun name ftyp sl
  | If(e, s1, s2) ->
      verify_implicit_return_basic_fun name ftyp [s1];
      verify_implicit_return_basic_fun name ftyp [s2]
  | For(vd, el, s) ->
      verify_implicit_return_basic_fun name ftyp [s]
  | While(e, s) ->
      verify_implicit_return_basic_fun name ftyp [s]
  | _ ->
    (match ftyp with
        Void -> ()
      | _ -> raise (SemError ("function " ^ name ^ " implicit return " ^
               "invalid type, expected: " ^  string_of_t ftyp)))

let rec verify_return_pass_end_turn_fun name body =
  let error_on_return_stmt = function
      Return(_) ->
        raise (SemError ("Return statement in turn function: " ^ name))
    | _ -> ()
  in
  let last_stmt =
    try
      List.hd (List.rev body)
    with Failure("hd") ->
      raise (SemError ("Turn function " ^ name ^
             " does not end with 'end' or 'pass' statement."))
  in
  List.iter error_on_return_stmt body;
  match last_stmt with
      Pass(fd, e) -> ()
    | End -> ()
    | Block(scope, sl) ->
        verify_return_pass_end_turn_fun name sl
    | If(e, s1, s2) ->
        verify_return_pass_end_turn_fun name [s1];
        verify_return_pass_end_turn_fun name [s2]
    | For(vd, el, s) ->
        verify_return_pass_end_turn_fun name [s]
    | While(e, s) ->
        verify_return_pass_end_turn_fun name [s]
    | _ -> raise (SemError ("Turn function " ^ name ^
                  " does not end with 'end' or 'pass' statement."))

let verify_implicit_return = function
    AssertFunc(f) ->
        () (* Implicit and explicit returns always Bool *)
  | BasicFunc(f) ->
        if f.turns_func then
          verify_return_pass_end_turn_fun f.fname f.body
        else
          verify_implicit_return_basic_fun f.fname f.ftype f.body

let check_basic_func env in_turn_section (f : Ast.basic_func_decl) =
    let scope' =
      { parent = Some(env.scope);
        variables = [];
        functions = [];
        groups = [];
        turns = [];
        ll_count = 0;
        elem_count = 0 } in
    let env' =
      { env with scope = scope';
        return_type = id_type_to_t f.Ast.ftype; } in
    let fl = List.map (fun v -> check_formal env' v) f.Ast.formals in
    let ll = List.map (fun dcl -> check_vdcl env' dcl) f.Ast.locals in
    let sl = List.map (fun s -> check_stmt env' s) f.Ast.body in
    let gname = (match env.partial_group_info with None -> "" | Some(g) -> g.group_name) in
    let fdecl =
      BasicFunc({ ftype = id_type_to_t f.Ast.ftype;
                  fname = f.Ast.fname;
                  formals = fl;
                  locals = ll;
                  body = sl;
                  turns_func = in_turn_section;
                  group_method = gname;
                  f_is_built_in = false })
    in
    verify_implicit_return fdecl;
    env.scope.functions <- fdecl :: env.scope.functions;
    fdecl

let rec verify_assert_func_stmt stmt =
  let msg_bool = "Assert function expression statment or return statement" ^
                 " must be of type boolean."
  in
  let msg_stmt = "Assert function statment cannot be " in
  match stmt with
      Block(scope, sl) -> List.iter verify_assert_func_stmt sl
    | Expression(e) -> require_bool e msg_bool
    | Return(e) -> require_bool e msg_bool
    | Break -> ()
    | Continue -> ()
    | End -> raise (SemError (msg_stmt ^ "end."))
    | Pass(_, _) -> raise (SemError (msg_stmt ^ "pass."))
    | If(e, s1, s2) ->
        verify_assert_func_stmt s1;
        verify_assert_func_stmt s2
    | For(vd, el, s) -> verify_assert_func_stmt s
    | While(e, s) -> verify_assert_func_stmt s

let check_assert_func env in_turn_section (f : Ast.assert_decl) =
  let scope' =
    { parent = Some(env.scope);
      variables = [];
      functions = [];
      groups = [];
      turns = [];
      ll_count = 0;
      elem_count = 0 } in
  let env' =
    { env with scope = scope';
      return_type = Bool; } in
  let fl = List.map (fun v -> check_formal env' v) f.Ast.formals in
  let ll = List.map (fun dcl -> check_vdcl env' dcl) f.Ast.locals in
  let sl = List.map (fun s -> check_stmt env' s) f.Ast.body in
  let ret_stmt = Return(BoolLiteral(True, ""), Bool) in
  let sl = ret_stmt :: List.rev sl in
  let sl = List. rev sl in
  let gname = (match env.partial_group_info with None -> "" | Some(g) -> g.group_name) in
  let fdecl =
    AssertFunc({ aname = f.Ast.fname;
                 aformals = fl;
                 alocals = ll;
                 abody = sl ;
                 a_turns_func = in_turn_section;
                 a_group_method = gname;
                 a_is_built_in = false })
  in
  List.iter verify_assert_func_stmt sl;
  env.scope.functions <- fdecl :: env.scope.functions;
  fdecl

let check_function env in_turn_section = function
    Ast.BasicFunc(f) ->
      verify_not_redeclaring env.scope f.Ast.fname;
      check_basic_func env in_turn_section f
  | Ast.AssertFunc(f) ->
      verify_not_redeclaring env.scope f.Ast.fname;
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
                    let fdcl = par_init in
                    let par_init = match par_init with
                        BasicFunc(f) -> f
                      | AssertFunc(f) ->
                          raise (SemError("Parent constructor cannot be an assert function"))
                    in
                    if List.length par_init.formals = List.length el then
                      verify_args fdcl par_init.formals el
                    else
                      raise (SemError("Number of constrctur variables for parent's constructor does not match")))
          | None ->
              match init_opt with
                  Some(init) -> ()
                | None -> () )
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

let check_attrib env v =
  let decl = check_vdcl_helper env v false in
  let name = decl.vname in
  let scope =
    (match env.partial_group_info with
        None -> raise (SemError "Internal error: check_attrib called outside of group definition.")
      | Some(info) -> info.symbols) in
  verify_not_redeclaring scope name;
  scope.variables <- decl :: scope.variables;
  decl

let check_method env new_fun =
  let fdcl = check_function env false new_fun in
  let name = match fdcl with
      BasicFunc(f) -> f.fname
    | AssertFunc(f) -> f.aname
  in
  let scope =
    (match env.partial_group_info with
        None -> raise (SemError "Internal error: check_attrib called outside of group definition.")
      | Some(info) -> info.symbols) in
  verify_not_redeclaring scope name;
  scope.functions <- fdcl :: scope.functions;
  fdcl

let add_parent_init parent par_actuals name methods = function
    Some(init_fun) ->
      (match init_fun with
          BasicFunc(f) ->
            if f.ftype = Group(name, None) then
              methods
            else
              raise (SemError ("Group " ^ name ^ " __init__ function has type not equal to itself"))
        | AssertFunc(f) ->
            raise (SemError ("Group " ^ name ^ " __init__ function not a basic function")))
  | None ->
    (match parent with
        None -> raise (SemError "No parent but using parent __init__")
      | Some(par) ->
        let par_init =
          try
            find_init_func par.methods
          with Not_found ->
            raise (SemError ("Parent __init__ function not found for child: " ^
                             name))
        in
        let child_init =
          (match par_actuals with
            Some(el) ->
            (match par_init with
                AssertFunc(f) -> raise (SemError "Assert Function used as parent __init__")
              | BasicFunc(f) ->
                let dcls =
                  List.map2 (fun vdcl act -> {vdcl with vinit = Some(act)}) f.formals el
                in
                { ftype = Group(name, None);
                  fname = "__init__";
                  formals = [];
                  locals = dcls;
                  body = f.body;
                  turns_func = false;
                  group_method = name;
                  f_is_built_in = false })
          | None ->
            (match par_init with
                AssertFunc(f) -> raise (SemError "Assert Function used as parent __init__")
              | BasicFunc(f) ->
                { f with ftype = Group(name, None); group_method = name }))
      in
      BasicFunc(child_init) :: methods)

let verify_repr gname = function
    BasicFunc(f) ->
      if f.ftype <> Str then
        raise (SemError ("Group " ^ gname ^ " __repr__() does not have Str return type"))
      else if List.length f.formals > 0 then
        raise (SemError ("Group " ^ gname ^ " __repr__() has more than 0 formals"))
      else
        ()
  | AssertFunc(f) ->
      raise (SemError ("Group " ^ gname ^ " __repr__() cannot be an assert function"))

let rec find_repr gdcl =
  try
    List.find
      (fun f ->
        (match f with
           BasicFunc(x) -> x.fname = "__repr__"
         | AssertFunc(x) -> x.aname = "__repr__")) gdcl.methods
  with Not_found ->
    match gdcl.extends with
        Some(par) -> find_repr par
      | None -> raise Not_found

let rec instance_of name gdcl =
  if gdcl.gname = name then
    true
  else
    match gdcl.extends with
      None -> false
    | Some(par) -> instance_of name par

let check_for_repr env gdcl =
  try
    (* search locally first *)
    let repr = find_repr { gdcl with extends = None } in
    verify_repr gdcl.gname repr;
    gdcl
  with Not_found ->
  try
    (* not in group, search any parents *)
    let repr = find_repr gdcl in
    let built_in = match repr with
        BasicFunc(f) -> f.f_is_built_in
      | AssertFunc(f) -> raise (SemError ("Group " ^ gdcl.gname ^ " __repr__() cannot be an assert function"))
    in
    let not_a_piece = not (instance_of "Piece" gdcl) in
    let repr =
      if built_in && not_a_piece then
        let s = "<Group " ^ gdcl.gname ^ " instance>" in
        let body = Return(StrLiteral(s, ""), Str) in
        BasicFunc({ ftype = Str;
                    fname = "__repr__";
                    formals = [];
                    locals = [];
                    body = [body];
                    turns_func = false;
                    group_method = gdcl.gname;
                    f_is_built_in = true})
      else
        repr
    in
    verify_repr gdcl.gname repr;
    { gdcl with methods = repr :: gdcl.methods }
  with Not_found ->
    let s = "<Group " ^ gdcl.gname ^ " instance>" in
    let body = Return(StrLiteral(s, ""), Str) in
    let repr =
      { ftype = Str;
        fname = "__repr__";
        formals = [];
        locals = [];
        body = [body];
        turns_func = false;
        group_method = gdcl.gname;
        f_is_built_in = true }
    in
    { gdcl with methods = BasicFunc(repr) :: gdcl.methods }

let rec check_group env g =
  let parent = match g.Ast.extends with
      Some(fe) ->
        let par, _ = check_field env [] fe in
        (match par with
            Grp(p) -> Some(p)
          | _ -> raise (SemError ("Parent is not a group")))
    | None ->
        if g.Ast.gname = "Object" then
          None
        else
          raise (SemError "Must inherit from Object or another group")
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
        turns = [];
        ll_count = 0;
        elem_count = 0 } in
  let partial_scope = {scope' with parent = None} in
  let info =
    { group_name = g.Ast.gname;
      symbols = partial_scope;
      par = parent } in
  let env' =
    { env with scope = scope';
      return_type = Void;
      partial_group_info = Some(info) } in
  let attribs = List.map (check_attrib env') g.Ast.attributes in
  let attribs =
    (match parent with
       Some(par) -> verify_attributes par.attributes attribs
     | None -> attribs) in
  let methods = List.map (check_method env') g.Ast.methods in
  let init_opt = try
    Some(find_init_func methods)
  with Not_found ->
    None
  in
  let methods = add_parent_init parent par_actuals g.Ast.gname methods init_opt in
  verify_extends parent par_actuals init_opt;
  let gdecl =
    { gname = g.Ast.gname;
      extends = parent;
      par_actuals = par_actuals;
      attributes = attribs;
      methods = methods }
  in
  let gdecl = check_for_repr env gdecl in
  verify_not_redeclaring env.scope gdecl.gname;
  env.scope.groups <- gdecl :: env.scope.groups;
  gdecl

let check_setup env setup_section =
  let vars, funcs, groups = setup_section in
  List.map (check_vdcl env) vars,
  List.map (check_function env false) funcs,
  List.map (check_group env) groups

let rec gather_turn_names env = function
    [] -> ()
  | Ast.BasicFunc(t) :: rest ->
      env.scope.turns <- t.Ast.fname :: env.scope.turns;
      gather_turn_names env rest
  | Ast.AssertFunc(t) :: rest ->
      raise (SemError ("Assert function cannot be a turns function: " ^ t.Ast.fname ))

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
      variables = Stdlib.vars;
      functions = Stdlib.funcs;
      groups = Stdlib.grps;
      turns = [];
      ll_count = 0;
      elem_count = 0 } in
  let env =
    { scope = symbols;
      return_type = Void;
      in_loop = false;
      partial_group_info = None } in
  let setup_section, turns_section = program in
  let setup_section = check_setup env setup_section in
  let turns_section = check_turns env turns_section in
  let turns_section = List.map (fix_pass_stmts turns_section) turns_section in
  setup_section, turns_section

