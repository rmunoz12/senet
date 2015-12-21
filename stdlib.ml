open Types

let funcs =
  let s =
    { vname = "print_arg";
      vtype = Str;
      vinit = None;
      vloop = false }
  in
  let b = { s with vtype = Bool } in
  let i = { s with vtype = Int } in
  let g = { s with vtype = Group("", None) } in
  let print_str =
    { ftype = Void;
      fname = "print";
      formals = [s];
      locals = [];
      body = [];
      turns_func = false;
      group_method = "";
      f_is_built_in = true }
  in
  let print_bool = { print_str with formals = [b] } in
  let print_int = { print_str with formals = [i] } in
  let print_group = { print_str with formals = [g] } in
  let read = { print_str with fname = "read"; ftype = Str; formals = [i] } in
  let clear = { print_str with fname = "clear_input"; ftype = Void; formals = [] } in
  let stoi = { print_str with fname = "stoi"; ftype = Int; formals = [s] } in
  let exit = { print_str with fname = "exit"; ftype = Void; formals = [] } in
  [BasicFunc(print_str); BasicFunc(print_bool); BasicFunc(print_int);
   BasicFunc(print_group); BasicFunc(read); BasicFunc(stoi); BasicFunc(exit);
   BasicFunc(clear)]

let vars =
  let init = IntLiteral(0, "") in
  let v = { vname = "PLAYER_ON_MOVE" ; vinit = Some(init, Int) ;
            vtype = Int; vloop = false }
  in
  [v]

let base_init name =
  let stmt = Return(Field(This), Group(name, None)) in
  { ftype = Group(name, None);
    fname = "__init__";
    formals = [];
    locals = [];
    body = [stmt];
    turns_func = false;
    group_method = name;
    f_is_built_in = true }

let base_repr name =
  let s = "<Group " ^ name ^ " instance>" in
  let body = Return(StrLiteral(s, ""), Str) in
  BasicFunc({ ftype = Str;
              fname = "__repr__";
              formals = [];
              locals = [];
              body = [body];
              turns_func = false;
              group_method = name;
              f_is_built_in = true})

let obj =
  let name = "Object" in
  { gname = name;
    extends = None;
    par_actuals = None;
    attributes = [];
    methods = [BasicFunc(base_init name)] }

let board =
  let v =
    { vname = "cells"; vtype = List_t(Group("Piece", None));
      vinit = None; vloop = false }
  in
  let occupied = { v with vname ="occupied"; vtype = List_t(Bool) } in
  let attr = [v; occupied] in
  let v = { v with vname = "x"; vtype = Int } in
  let remove =
    { ftype = Bool; fname = "remove"; formals = [v]; locals = []; body = [];
      turns_func = false; group_method = "Board"; f_is_built_in = true }
  in
  let owns = { remove with ftype = Int; fname = "owns" } in
  let v = { v with vname = "l"; vtype = List_t(Int) } in
  let toi = { owns with fname = "toi"; formals = [v] } in
  let tol = { owns with ftype = List_t(Int); fname = "tol" } in
  let full = { owns with ftype = Bool; fname = "full"; formals = [] } in
  let x = { v with vname = "x"; vtype = Int } in
  let p = { v with vname = "p"; vtype = Group("Piece", None) } in
  let place =
    { ftype = Bool; fname = "place"; formals = [p; x]; locals = []; body = [];
      turns_func = false; group_method = "Board"; f_is_built_in = true }
  in
  let meth =
    [BasicFunc(remove); BasicFunc(owns); BasicFunc(owns);
     BasicFunc(toi); BasicFunc(tol); BasicFunc(base_init "Board");
     base_repr "Board"; BasicFunc(full); BasicFunc(place)]
  in
  { obj with gname = "Board"; extends = Some(obj);
    attributes = attr; methods = meth }

let piece =
  let v = { vname = ""; vtype = Void; vinit = None; vloop = false } in
  let owner = { v with vname = "owner"; vtype = Int } in
  let fixed = { v with vname = "fixed"; vtype = Bool } in
  let s = { v with vname = "s"; vtype = Str } in
  let this_dummy =
    { vname = "this"; vtype = Group("Loop", None);
      vinit = None; vloop = false }
  in
  let stmts =
    [Expression(Assign(Attrib(this_dummy, s), (Field(Var(s)), Int)), Int);
     Return(Field(This), Group("Piece", None))]
  in
  let init = base_init "Piece" in
  let init = { init with body = stmts; formals = [s] } in
  let stmts =
    [Return(Field(Attrib(this_dummy, s)), Str)]
  in
  let repr = base_repr "Piece" in
  let repr =
    match repr with
      BasicFunc(f) -> { f with body = stmts }
    | _ -> raise (Failure ("__repr__ for piece matched to non-basicfunc"))
  in
  { obj with gname = "Piece"; extends = Some(obj);
    attributes = [owner; fixed; s];
    methods = [BasicFunc(base_init "Piece"); BasicFunc(init); BasicFunc(repr)] }

let boards_lib =
  let x = { vname = "x"; vtype = Int; vinit = None; vloop = false } in
  let y = { x with vname = "y" } in
  let this_dummy =
    { vname = "this"; vtype = Group("Rect", None);
      vinit = None; vloop = false }
  in
  let init_cells =
    { ftype = Void;
      fname = "INIT_CELLS";
      formals = [this_dummy; x];
      locals = [];
      body = [];
      turns_func = false;
      group_method = "Board";
      f_is_built_in = true }
  in
  let stmts =
    [Expression(Assign(Attrib(this_dummy, x), (Field(Var(x)), Int)), Int);
     Expression(Assign(Attrib(this_dummy, y), (Field(Var(y)), Int)), Int);
     Expression(Call(Some(this_dummy), BasicFunc(init_cells),
                     [Binop((Field(Var(x)), Int), Mult,
                            (Field(Var(y)), Int)), Int]), Void);
     Return(Field(This), Group("Rect", None))]
  in
  let init = base_init "Rect" in
  let init = { init with formals = [x; y]; body = stmts } in
  let coord = { x with vname = "coord"; vtype = List_t(Int) } in
  let toi =
    {init with ftype = Int; fname = "toi"; formals = [coord]; body = [] }
  in
  let tol =
    {init with ftype = List_t(Int); fname = "tol"; formals = [x]; body = [] }
  in
  let rect =
    { board with gname = "Rect"; extends = Some(board); attributes = [x; y];
      methods = [BasicFunc(init); base_repr "Rect"; BasicFunc(toi);
                 BasicFunc(tol)] }
  in
  let this_dummy =
    { vname = "this"; vtype = Group("Loop", None);
      vinit = None; vloop = false }
  in
  let stmts =
    [Expression(Assign(Attrib(this_dummy, x), (Field(Var(x)), Int)), Int);
     Expression(Call(Some(this_dummy), BasicFunc(init_cells),
                     [(Field(Var(x)), Int)]), Void);
     Return(Field(This), Group("Loop", None))]
  in
  let init = base_init "Loop" in
  let init = { init with formals = [x]; body = stmts } in
  let coord = { x with vname = "coord"; vtype = List_t(Int) } in
  let toi =
    {init with ftype = Int; fname = "toi"; formals = [coord]; body = [] }
  in
  let tol =
    {init with ftype = List_t(Int); fname = "tol"; formals = [x]; body = [] }
  in
  let loop =
    { rect with gname = "Loop"; attributes = [x];
      methods = [BasicFunc(init); base_repr "Loop"; BasicFunc(toi); BasicFunc(tol)] }
  in
  let this_dummy =
    { vname = "this"; vtype = Group("Line", None);
      vinit = None; vloop = false }
  in
  let init = base_init "Line" in
  let stmts =
    [Expression(Assign(Attrib(this_dummy, x), (Field(Var(x)), Int)), Int);
     Expression(Call(Some(this_dummy), BasicFunc(init_cells),
                     [(Field(Var(x)), Int)]), Void);
     Return(Field(This), Group("Line", None))]
  in
  let init = { init with formals = [x]; body = stmts } in
  let coord = { x with vname = "coord"; vtype = List_t(Int) } in
  let toi =
    {init with ftype = Int; fname = "toi"; formals = [coord]; body = [] }
  in
  let tol =
    {init with ftype = List_t(Int); fname = "tol"; formals = [x]; body = [] }
  in
  let line =
    { loop with gname = "Line";
      methods = [BasicFunc(init); base_repr "Loop"; BasicFunc(toi); BasicFunc(tol)] }
  in
  let this_dummy =
    { vname = "this"; vtype = Group("Hex", None);
      vinit = None; vloop = false }
  in
  let stmts =
    [Expression(Assign(Attrib(this_dummy, x), (Field(Var(x)), Int)), Int);
     Expression(Call(Some(this_dummy), BasicFunc(init_cells),
                     [(Field(Var(x)), Int)]), Void);
     Return(Field(This), Group("Hex", None))]
  in
  let init = base_init "Hex" in
  let init = { init with formals = [x]; body = stmts } in
  let coord = { x with vname = "coord"; vtype = List_t(Int) } in
  let toi =
    {init with ftype = Int; fname = "toi"; formals = [coord]; body = [] }
  in
  let tol =
    {init with ftype = List_t(Int); fname = "tol"; formals = [x]; body = [] }
  in
  let hex =
    { loop with gname = "Hex";
      methods = [BasicFunc(init); base_repr "Loop"; BasicFunc(toi); BasicFunc(tol)] }
  in
  [rect; loop; line; hex]

let grps =
  [obj; board; piece] @ boards_lib
